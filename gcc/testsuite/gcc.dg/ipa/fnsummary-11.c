/* { dg-do run } */
/* { dg-options "-O2 -fno-partial-inlining -fdump-ipa-fnsummary-details -fdump-ipa-inline-details" } */

struct Node
{
  const char *value;
  unsigned type;
  unsigned id;
  struct Node *next;
};

enum NodeType { NAME = 1, NUMBER = 2, OP = 3 };

__attribute__((noipa)) static int
is_builtin_type (const char *v)
{
  static const char *const types[] = {
    "int", "char", "long", "short", "float", "double", "void", "bool"
  };
  for (unsigned i = 0; i < sizeof (types) / sizeof (types[0]); ++i)
    {
      const char *a = v;
      const char *b = types[i];
      while (*a && *a == *b)
	{
	  ++a;
	  ++b;
	}
      if (*a == *b)
	return 1;
    }
  return 0;
}

static inline int
match_specifier (const struct Node *node, const char *p)
{
  switch (p[1])
    {
    case 'n':
      if (p[2] == 'a')
	{
	  if (node->type != NAME)
	    return 0;
	  unsigned char ch = (unsigned char) node->value[0];
	  return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z');
	}
      else
	{
	  const char *v = node->value;
	  if (node->type != NUMBER)
	    return 0;
	  if (v[0] == '0' && (v[1] == 'x' || v[1] == 'X'))
	    return 1;
	  if (v[0] >= '0' && v[0] <= '9')
	    return 1;
	  if (v[0] == '-' && v[1] >= '0' && v[1] <= '9')
	    return 1;
	  return 0;
	}
    case 't':
      {
	const char *v = node->value;
	if (node->type != NAME)
	  return 0;
	if (v[0] >= 'A' && v[0] <= 'Z')
	  return 1;
	return is_builtin_type (v);
      }
    default:
      return 0;
    }
}

static inline int
match_bracket (const struct Node *node, const char *p)
{
  if (node->value[0] == '\0' || node->value[1] != '\0')
    return 0;
  int negated = (p[1] == '^');
  const char *c = negated ? p + 2 : p + 1;
  int found = 0;
  while (*c && *c != ' ' && *c != ']')
    {
      if (*c == node->value[0])
	found = 1;
      ++c;
    }
  if (*c != ']')
    return 0;
  return negated ? !found : found;
}

static inline int
match_negation (const struct Node *node, const char *p)
{
  const char *s = node->value;
  const char *w = p + 2;
  for (;;)
    {
      if (*s != *w)
	{
	  if (*s == '\0' && (*w == ' ' || *w == '\0'))
	    return 0;
	  return 1;
	}
      if (*s == '\0')
	return 0;
      ++s;
      ++w;
    }
}

static inline int
match_alternatives (const struct Node *node, const char *p)
{
  const char *needle = node->value;
  const char *np = needle;
  for (;;)
    {
      if (*p == '|')
	{
	  if (*np == '\0')
	    return 1;
	  np = needle;
	  ++p;
	}
      else if (*np == *p)
	{
	  if (*np == '\0' || *p == ' ')
	    return 1;
	  ++np;
	  ++p;
	}
      else if (*p == ' ' || *p == '\0')
	return *np == '\0';
      else
	{
	  np = needle;
	  do
	    {
	      ++p;
	      if (*p == ' ' || *p == '\0')
		return 0;
	    }
	  while (*p != '|');
	  ++p;
	}
    }
}

static inline int
match (const struct Node *node, const char *pattern)
{
  if (!*pattern)
    return 1;

  const char *p = pattern;
  for (;;)
    {
      while (*p == ' ')
	++p;
      if (*p == '\0')
	break;

      if (!node)
	{
	  if (p[0] == '!' && p[1] == '!' && p[2] != '\0')
	    {
	      while (*p && *p != ' ')
		++p;
	      continue;
	    }
	  return 0;
	}

      switch (*p)
	{
	case '%':
	  if (!match_specifier (node, p))
	    return 0;
	  break;
	case '[':
	  if (!match_bracket (node, p))
	    return 0;
	  break;
	case '!':
	  if (p[1] == '!' && p[2] != '\0')
	    {
	      if (!match_negation (node, p))
		return 0;
	      break;
	    }
	  /* Fall through.  */
	default:
	  if (!match_alternatives (node, p))
	    return 0;
	  break;
	}

      while (*p && *p != ' ')
	++p;
      if (*p == '\0')
	break;
      node = node->next;
    }
  return 1;
}

__attribute__((noinline)) int
caller_specifier (const struct Node *n)
{
  return match (n, "%name% :: %type%");
}

__attribute__((noinline)) int
caller_bracket (const struct Node *n)
{
  return match (n, "[;,{}]");
}

__attribute__((noinline)) int
caller_negation (const struct Node *n)
{
  return match (n, "!!else");
}

__attribute__((noinline)) int
caller_alternatives (const struct Node *n)
{
  return match (n, "class|struct|union");
}

__attribute__((noinline)) int
caller_mixed (const struct Node *n)
{
  return match (n, "class|struct %name%");
}

int
main (void)
{
  struct Node n3 = { "int", NAME, 0, 0 };
  struct Node n2 = { "::", OP, 0, &n3 };
  struct Node n1 = { "foo", NAME, 42, &n2 };
  struct Node br = { ";", OP, 0, 0 };
  struct Node kw = { "struct", NAME, 0, &n1 };
  struct Node kw_else = { "else", NAME, 0, 0 };

  if (!caller_specifier (&n1))
    __builtin_abort ();
  if (caller_specifier (&n2))
    __builtin_abort ();
  if (!caller_bracket (&br))
    __builtin_abort ();
  if (caller_bracket (&n1))
    __builtin_abort ();
  if (!caller_negation (&n1))
    __builtin_abort ();
  if (caller_negation (&kw_else))
    __builtin_abort ();
  if (!caller_alternatives (&kw))
    __builtin_abort ();
  if (caller_alternatives (&n2))
    __builtin_abort ();
  if (!caller_mixed (&kw))
    __builtin_abort ();
  if (caller_mixed (&n2))
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-ipa-dump "nonconst:\[^\n\]*op1\\\[ref maybe_modified unknown_offset\\\] != 32\[^\n\]*op1\\\[ref maybe_modified unknown_offset\\\] == 32" "fnsummary" } } */
/* { dg-final { scan-ipa-dump "nonconst:\[^\n\]*op1\\\[ref maybe_modified unknown_offset\\\] == 33 \\|\\| op1\\\[ref maybe_modified unknown_offset\\\] == 37 \\|\\| op1\\\[ref maybe_modified unknown_offset\\\] == 91" "fnsummary" } } */
/* { dg-final { scan-ipa-dump "nonconst:\[^\n\]*op1\\\[ref unknown_offset\\\]\[^\n\]*== 124" "fnsummary" } } */
/* { dg-final { scan-ipa-dump "Known to be false:\[^\n\]*op1\\\[ref maybe_modified unknown_offset\\\] == 32,\[^\n\]*== 33,\[^\n\]*== 37,\[^\n\]*== 91" "inline" } } */
/* { dg-final { scan-ipa-dump "Inlined match\[^\n\]* into caller_alternatives/" "inline" } } */
