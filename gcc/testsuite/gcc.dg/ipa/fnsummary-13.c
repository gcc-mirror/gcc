/* { dg-do run } */
/* { dg-options "-O2 -fno-partial-inlining -fdump-ipa-fnsummary-details -fdump-ipa-inline-details" } */

static inline const char *
find_char (const char *s, char c)
{
  for (; *s; ++s)
    if (*s == c)
      return s;
  return 0;
}

static inline int
match_pattern (int target, const char *pattern)
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

      if (*p == '[')
	{
	  const char *end = find_char (p + 1, ']');
	  if (!end)
	    return 0;
	  int matched = 0;
	  for (const char *c = p + 1; c < end; ++c)
	    if (*c == target)
	      matched = 1;
	  if (!matched)
	    return 0;
	  p = end + 1;
	}
      else if (*p == '!')
	{
	  if (p[1] == '!')
	    p += 2;
	  if (*p == target)
	    return 0;
	  while (*p && *p != ' ')
	    ++p;
	}
      else
	{
	  if (*p != target)
	    return 0;
	  while (*p && *p != ' ')
	    ++p;
	}

      if (*p == '\0')
	break;
    }
  return 1;
}

__attribute__((noinline)) int
caller_bang (int target)
{
  return match_pattern (target, "!!x");
}

__attribute__((noinline)) int
caller_bracket (int target)
{
  return match_pattern (target, "[ab] [bc]");
}

__attribute__((noinline)) int
caller_plain (int target)
{
  return match_pattern (target, "a");
}

__attribute__((optimize ("no-delete-null-pointer-checks"), noinline)) int
check_hash_null_phi (int dummy, const char *pattern)
{
  (void) dummy;
  const char *end = find_char (pattern, ']');
  if (!end)
    return 0;
  if (end[1] == '#')
    return 1;
  return 0;
}

__attribute__((noinline)) int
caller_no_delete_null (void)
{
  return check_hash_null_phi (0, "a]#")
	 + check_hash_null_phi (0, "a]b")
	 + check_hash_null_phi (0, "abc");
}

int
main (void)
{
  if (!caller_bang ('y'))
    __builtin_abort ();
  if (caller_bang ('x'))
    __builtin_abort ();
  if (!caller_bracket ('b'))
    __builtin_abort ();
  if (caller_bracket ('z'))
    __builtin_abort ();
  if (!caller_plain ('a'))
    __builtin_abort ();
  if (caller_plain ('b'))
    __builtin_abort ();
  if (caller_no_delete_null () != 1)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-ipa-dump "nonconst:\[^\n\]*op1\\\[ref unknown_offset\\\] != 32\[^\n\]*op1\\\[ref unknown_offset\\\] == 32" "fnsummary" } } */
/* { dg-final { scan-ipa-dump "nonconst:\[^\n\]*op1\\\[ref unknown_offset\\\] != 91\[^\n\]*op1\\\[ref unknown_offset\\\] == 91" "fnsummary" } } */
/* { dg-final { scan-ipa-dump "nonconst:\[^\n\]*op1\\\[ref unknown_offset\\\] != 93\[^\n\]*op1\\\[ref unknown_offset\\\] == 93" "fnsummary" } } */
/* { dg-final { scan-ipa-dump "nonconst:\[^\n\]*op1\\\[ref unknown_offset\\\] != 33\[^\n\]*op1\\\[ref unknown_offset\\\] == 33" "fnsummary" } } */
/* { dg-final { scan-ipa-dump "Known to be false:\[^\n\]*op1\\\[ref unknown_offset\\\] == 32\[^\n\]*op1\\\[ref unknown_offset\\\] == 91\[^\n\]*op1\\\[ref unknown_offset\\\] == 93\[^\n\]*op1\\\[ref unknown_offset\\\] == 33" "inline" } } */
/* { dg-final { scan-ipa-dump-not "op1\\\[ref unknown_offset\\\] == 35" "fnsummary" } } */
