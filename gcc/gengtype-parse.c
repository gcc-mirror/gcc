/* Process source files and output type information.
   Copyright (C) 2006, 2007 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "bconfig.h"
#include "system.h"
#include "gengtype.h"

/* This is a simple recursive-descent parser which understands a subset of
   the C type grammar.

   Rule functions are suffixed _seq if they scan a sequence of items;
   _opt if they may consume zero tokens; _seqopt if both are true.  The
   "consume_" prefix indicates that a sequence of tokens is parsed for
   syntactic correctness and then thrown away.  */

/* Simple one-token lookahead mechanism.  */

struct token
{
  const char *value;
  int code;
  bool valid;
};
static struct token T;

/* Retrieve the code of the current token; if there is no current token,
   get the next one from the lexer.  */
static inline int
token (void)
{
  if (!T.valid)
    {
      T.code = yylex (&T.value);
      T.valid = true;
    }
  return T.code;
}

/* Retrieve the value of the current token (if any) and mark it consumed.
   The next call to token() will get another token from the lexer.  */
static inline const char *
advance (void)
{
  T.valid = false;
  return T.value;
}

/* Diagnostics.  */

/* This array is indexed by the token code minus CHAR_TOKEN_OFFSET.  */
static const char *const token_names[] = {
  "GTY",
  "typedef",
  "extern",
  "static",
  "union",
  "struct",
  "enum",
  "VEC",
  "DEF_VEC_[OP]",
  "DEF_VEC_I",
  "DEF_VEC_ALLOC_[IOP]",
  "...",
  "ptr_alias",
  "nested_ptr",
  "a param<N>_is option",
  "a number",
  "a scalar type",
  "an identifier",
  "a string constant",
  "a character constant",
  "an array declarator",
};

/* This array is indexed by token code minus FIRST_TOKEN_WITH_VALUE.  */
static const char *const token_value_format[] = {
  "%s",
  "'%s'",
  "'%s'",
  "'%s'",
  "'\"%s\"'",
  "\"'%s'\"",
  "'[%s]'",
};

/* Produce a printable representation for a token defined by CODE and
   VALUE.  This sometimes returns pointers into malloc memory and
   sometimes not, therefore it is unsafe to free the pointer it
   returns, so that memory is leaked.  This does not matter, as this
   function is only used for diagnostics, and in a successful run of
   the program there will be none.  */
static const char *
print_token (int code, const char *value)
{
  if (code < CHAR_TOKEN_OFFSET)
    return xasprintf ("'%c'", code);
  else if (code < FIRST_TOKEN_WITH_VALUE)
    return xasprintf ("'%s'", token_names[code - CHAR_TOKEN_OFFSET]);
  else if (!value)
    return token_names[code - CHAR_TOKEN_OFFSET]; /* don't quote these */
  else
    return xasprintf (token_value_format[code - FIRST_TOKEN_WITH_VALUE],
		      value);
}

/* Convenience wrapper around print_token which produces the printable
   representation of the current token.  */
static inline const char *
print_cur_token (void)
{
  return print_token (T.code, T.value);
}

/* Report a parse error on the current line, with diagnostic MSG.
   Behaves as standard printf with respect to additional arguments and
   format escapes.  */
static void ATTRIBUTE_PRINTF_1
parse_error (const char *msg, ...)
{
  va_list ap;

  fprintf (stderr, "%s:%d: parse error: ", lexer_line.file, lexer_line.line);

  va_start (ap, msg);
  vfprintf (stderr, msg, ap);
  va_end (ap);

  hit_error = true;
}

/* If the next token does not have code T, report a parse error; otherwise
   return the token's value.  */
static const char *
require (int t)
{
  int u = token ();
  const char *v = advance ();
  if (u != t)
    {
      parse_error ("expected %s, have %s",
		   print_token (t, 0), print_token (u, v));
      return 0;
    }
  return v;
}

/* If the next token does not have one of the codes T1 or T2, report a
   parse error; otherwise return the token's value.  */
static const char *
require2 (int t1, int t2)
{
  int u = token ();
  const char *v = advance ();
  if (u != t1 && u != t2)
    {
      parse_error ("expected %s or %s, have %s",
		   print_token (t1, 0), print_token (t2, 0),
		   print_token (u, v));
      return 0;
    }
  return v;
}

/* Near-terminals.  */

/* C-style string constant concatenation: STRING+
   Bare STRING should appear nowhere else in this file.  */
static const char *
string_seq (void)
{
  const char *s1, *s2;
  size_t l1, l2;
  char *buf;

  s1 = require (STRING);
  if (s1 == 0)
    return "";
  while (token () == STRING)
    {
      s2 = advance ();

      l1 = strlen (s1);
      l2 = strlen (s2);
      buf = XRESIZEVEC (char, CONST_CAST(char *, s1), l1 + l2 + 1);
      memcpy (buf + l1, s2, l2 + 1);
      XDELETE (CONST_CAST (char *, s2));
      s1 = buf;
    }
  return s1;
}

/* typedef_name: either an ID, or VEC(x,y) which is translated to VEC_x_y.
   Use only where VEC(x,y) is legitimate, i.e. in positions where a
   typedef name may appear.  */
static const char *
typedef_name (void)
{
  if (token () == VEC_TOKEN)
    {
      const char *c1, *c2, *r;
      advance ();
      require ('(');
      c1 = require2 (ID, SCALAR);
      require (',');
      c2 = require (ID);
      require (')');
      r = concat ("VEC_", c1, "_", c2, (char *)0);
      free (CONST_CAST (char *, c1));
      free (CONST_CAST (char *, c2));
      return r;
    }
  else
    return require (ID);
}

/* Absorb a sequence of tokens delimited by balanced ()[]{}.  */
static void
consume_balanced (int opener, int closer)
{
  require (opener);
  for (;;)
    switch (token ())
      {
      default: advance (); break;
      case '(': consume_balanced ('(',')'); break;
      case '[': consume_balanced ('[',']'); break;
      case '{': consume_balanced ('{','}'); break;

      case '}':
      case ']':
      case ')':
	if (token () != closer)
	  parse_error ("unbalanced delimiters - expected '%c', have '%c'",
		       closer, token ());
	advance ();
	return;

      case EOF_TOKEN:
	parse_error ("unexpected end of file within %c%c-delimited construct",
		     opener, closer);
	return;
      }
}

/* Absorb a sequence of tokens, possibly including ()[]{}-delimited
   expressions, until we encounter a semicolon outside any such
   delimiters; absorb that too.  If IMMEDIATE is true, it is an error
   if the semicolon is not the first token encountered.  */
static void
consume_until_semi (bool immediate)
{
  if (immediate && token () != ';')
    require (';');
  for (;;)
    switch (token ())
      {
      case ';':	advance (); return;
      default:	advance (); break;

      case '(':	consume_balanced ('(',')'); break;
      case '[': consume_balanced ('[',']'); break;
      case '{':	consume_balanced ('{','}'); break;

      case '}':
      case ']':
      case ')':
	parse_error ("unmatched '%c' while scanning for ';'", token ());
	return;

      case EOF_TOKEN:
	parse_error ("unexpected end of file while scanning for ';'");
	return;
      }
}

/* Absorb a sequence of tokens, possibly including ()[]{}-delimited
   expressions, until we encounter a comma or semicolon outside any
   such delimiters; absorb that too.  If IMMEDIATE is true, it is an
   error if the comma or semicolon is not the first token encountered.
   Returns true if the loop ended with a comma.  */
static bool
consume_until_comma_or_semi (bool immediate)
{
  if (immediate && token () != ',' && token () != ';')
    require2 (',', ';');
  for (;;)
    switch (token ())
      {
      case ',':	advance (); return true;
      case ';':	advance (); return false;
      default:	advance (); break;

      case '(':	consume_balanced ('(',')'); break;
      case '[': consume_balanced ('[',']'); break;
      case '{':	consume_balanced ('{','}'); break;

      case '}':
      case ']':
      case ')':
	parse_error ("unmatched '%s' while scanning for ',' or ';'",
		     print_cur_token ());
	return false;

      case EOF_TOKEN:
	parse_error ("unexpected end of file while scanning for ',' or ';'");
	return false;
      }
}


/* GTY(()) option handling.  */
static type_p type (options_p *optsp, bool nested);

/* Optional parenthesized string: ('(' string_seq ')')? */
static options_p
str_optvalue_opt (options_p prev)
{
  const char *name = advance ();
  const char *value = "";
  if (token () == '(')
    {
      advance ();
      value = string_seq ();
      require (')');
    }
  return create_option (prev, name, value);
}

/* absdecl: type '*'*
   -- a vague approximation to what the C standard calls an abstract
   declarator.  The only kinds that are actually used are those that
   are just a bare type and those that have trailing pointer-stars.
   Further kinds should be implemented if and when they become
   necessary.  Used only within GTY(()) option values, therefore
   further GTY(()) tags within the type are invalid.  Note that the
   return value has already been run through adjust_field_type.  */
static type_p
absdecl (void)
{
  type_p ty;
  options_p opts;

  ty = type (&opts, true);
  while (token () == '*')
    {
      ty = create_pointer (ty);
      advance ();
    }

  if (opts)
    parse_error ("nested GTY(()) options are invalid");

  return adjust_field_type (ty, 0);
}

/* Type-option: '(' absdecl ')' */
static options_p
type_optvalue (options_p prev, const char *name)
{
  type_p ty;
  require ('(');
  ty = absdecl ();
  require (')');
  return create_option (prev, name, ty);
}

/* Nested pointer data: '(' type '*'* ',' string_seq ',' string_seq ')' */
static options_p
nestedptr_optvalue (options_p prev)
{
  type_p ty;
  const char *from, *to;

  require ('(');
  ty = absdecl ();
  require (',');
  to = string_seq ();
  require (',');
  from = string_seq ();
  require (')');

  return create_nested_ptr_option (prev, ty, to, from);
}

/* One GTY(()) option:
         ID str_optvalue_opt
       | PTR_ALIAS type_optvalue
       | PARAM_IS type_optvalue
       | NESTED_PTR nestedptr_optvalue
 */
static options_p
option (options_p prev)
{
  switch (token ())
    {
    case ID:
      return str_optvalue_opt (prev);

    case PTR_ALIAS:
      advance ();
      return type_optvalue (prev, "ptr_alias");

    case PARAM_IS:
      return type_optvalue (prev, advance ());

    case NESTED_PTR:
      advance ();
      return nestedptr_optvalue (prev);

    default:
      parse_error ("expected an option keyword, have %s",
		   print_cur_token ());
      advance ();
      return create_option (prev, "", "");
    }
}

/* One comma-separated list of options.  */
static options_p
option_seq (void)
{
  options_p o;

  o = option (0);
  while (token () == ',')
    {
      advance ();
      o = option (o);
    }
  return o;
}

/* GTY marker: 'GTY' '(' '(' option_seq? ')' ')' */
static options_p
gtymarker (void)
{
  options_p result = 0;
  require (GTY_TOKEN);
  require ('(');
  require ('(');
  if (token () != ')')
    result = option_seq ();
  require (')');
  require (')');
  return result;
}

/* Optional GTY marker.  */
static options_p
gtymarker_opt (void)
{
  if (token () != GTY_TOKEN)
    return 0;
  return gtymarker ();
}

/* Declarators. The logic here is largely lifted from c-parser.c.
   Note that we do not have to process abstract declarators, which can
   appear only in parameter type lists or casts (but see absdecl,
   above).  Also, type qualifiers are thrown out in gengtype-lex.l so
   we don't have to do it.  */

/* array_and_function_declarators_opt:
      \epsilon
      array_and_function_declarators_opt ARRAY
      array_and_function_declarators_opt '(' ... ')'

   where '...' indicates stuff we ignore except insofar as grouping
   symbols ()[]{} must balance.

   Subroutine of direct_declarator - do not use elsewhere. */

static type_p
array_and_function_declarators_opt (type_p ty)
{
  if (token () == ARRAY)
    {
      const char *array = advance ();
      return create_array (array_and_function_declarators_opt (ty), array);
    }
  else if (token () == '(')
    {
      /* We don't need exact types for functions.  */
      consume_balanced ('(', ')');
      array_and_function_declarators_opt (ty);
      return create_scalar_type ("function type");
    }
  else
    return ty;
}

static type_p inner_declarator (type_p, const char **, options_p *);

/* direct_declarator:
      '(' inner_declarator ')'
      gtymarker_opt ID array_and_function_declarators_opt

   Subroutine of declarator, mutually recursive with inner_declarator;
   do not use elsewhere.  */
static type_p
direct_declarator (type_p ty, const char **namep, options_p *optsp)
{
  /* The first token in a direct-declarator must be an ID, a
     GTY marker, or an open parenthesis.  */
  switch (token ())
    {
    case GTY_TOKEN:
      *optsp = gtymarker ();
      /* fall through */
    case ID:
      *namep = require (ID);
      break;

    case '(':
      advance ();
      ty = inner_declarator (ty, namep, optsp);
      require (')');
      break;

    default:
      parse_error ("expected '(', 'GTY', or an identifier, have %s",
		   print_cur_token ());
      /* Do _not_ advance if what we have is a close squiggle brace, as
	 we will get much better error recovery that way.  */
      if (token () != '}')
	advance ();
      return 0;
    }
  return array_and_function_declarators_opt (ty);
}

/* The difference between inner_declarator and declarator is in the
   handling of stars.  Consider this declaration:

      char * (*pfc) (void)

   It declares a pointer to a function that takes no arguments and
   returns a char*.  To construct the correct type for this
   declaration, the star outside the parentheses must be processed
   _before_ the function type, the star inside the parentheses must
   be processed _after_ the function type.  To accomplish this,
   declarator() creates pointers before recursing (it is actually
   coded as a while loop), whereas inner_declarator() recurses before
   creating pointers.  */

/* inner_declarator:
     '*' inner_declarator
     direct_declarator

   Mutually recursive subroutine of direct_declarator; do not use
   elsewhere.  */

static type_p
inner_declarator (type_p ty, const char **namep, options_p *optsp)
{
  if (token () == '*')
    {
      type_p inner;
      advance ();
      inner = inner_declarator (ty, namep, optsp);
      if (inner == 0)
	return 0;
      else
	return create_pointer (ty);
    }
  else
    return direct_declarator (ty, namep, optsp);
}

/* declarator: '*'+ direct_declarator

   This is the sole public interface to this part of the grammar.
   Arguments are the type known so far, a pointer to where the name
   may be stored, and a pointer to where GTY options may be stored.
   Returns the final type. */

static type_p
declarator (type_p ty, const char **namep, options_p *optsp)
{
  *namep = 0;
  *optsp = 0;
  while (token () == '*')
    {
      advance ();
      ty = create_pointer (ty);
    }
  return direct_declarator (ty, namep, optsp);
}

/* Types and declarations.  */

/* Structure field(s) declaration:
   (
       type bitfield ';'
     | type declarator bitfield? ( ',' declarator bitfield? )+ ';'
   )+

   Knows that such declarations must end with a close brace (or,
   erroneously, at EOF).
 */
static pair_p
struct_field_seq (void)
{
  pair_p f = 0;
  type_p ty, dty;
  options_p opts, dopts;
  const char *name;
  bool another;

  do
    {
      ty = type (&opts, true);
      /* Another piece of the IFCVT_EXTRA_FIELDS special case, see type().  */
      if (!ty && token () == '}')
	break;

      if (!ty || token () == ':')
	{
	  consume_until_semi (false);
	  continue;
	}

      do
	{
	  dty = declarator (ty, &name, &dopts);
	  /* There could be any number of weird things after the declarator,
	     notably bitfield declarations and __attribute__s.  If this
	     function returns true, the last thing was a comma, so we have
	     more than one declarator paired with the current type.  */
	  another = consume_until_comma_or_semi (false);

	  if (!dty)
	    continue;

	  if (opts && dopts)
	    parse_error ("two GTY(()) options for field %s", name);
	  if (opts && !dopts)
	    dopts = opts;

	  f = create_field_at (f, dty, name, dopts, &lexer_line);
	}
      while (another);
    }
  while (token () != '}' && token () != EOF_TOKEN);
  return nreverse_pairs (f);
}

/* This is called type(), but what it parses (sort of) is what C calls
   declaration-specifiers and specifier-qualifier-list:

     SCALAR
   | ID     // typedef
   | (STRUCT|UNION) ID? gtymarker? ( '{' gtymarker? struct_field_seq '}' )?
   | ENUM ID ( '{' ... '}' )?

   Returns a partial type; under some conditions (notably
   "struct foo GTY((...)) thing;") it may write an options
   structure to *OPTSP.
 */
static type_p
type (options_p *optsp, bool nested)
{
  const char *s;
  bool is_union;
  *optsp = 0;
  switch (token ())
    {
    case SCALAR:
      s = advance ();
      return create_scalar_type (s);

    case ID:
    case VEC_TOKEN:
      s = typedef_name ();
      return resolve_typedef (s, &lexer_line);

    case STRUCT:
    case UNION:
      {
	options_p opts = 0;

	is_union = (token() == UNION);
	advance ();

	if (token () == ID)
	  s = advance ();
	else
	  s = xasprintf ("anonymous:%s:%d", lexer_line.file, lexer_line.line);

	/* Top-level structures that are not explicitly tagged GTY(())
	   are treated as mere forward declarations.  This is because
	   there are a lot of structures that we don't need to know
	   about, and some of those have weird macro stuff in them
	   that we can't handle.  */
	if (nested || token () == GTY_TOKEN)
	  {
	    opts = gtymarker_opt ();
	    if (token () == '{')
	      {
		pair_p fields;
		advance ();
		fields = struct_field_seq ();
		require ('}');
		return new_structure (s, is_union, &lexer_line, fields, opts);
	      }
	  }
	else if (token () == '{')
	  consume_balanced ('{', '}');
	if (opts)
	  *optsp = opts;
	return find_structure (s, is_union);
      }

    case ENUM:
      advance ();
	if (token () == ID)
	  s = advance ();
	else
	  s = xasprintf ("anonymous:%s:%d", lexer_line.file, lexer_line.line);

      if (token () == '{')
	consume_balanced ('{','}');
      return create_scalar_type (s);

    default:
      parse_error ("expected a type specifier, have %s", print_cur_token ());
      advance ();
      return create_scalar_type ("erroneous type");
    }
}

/* Top level constructs.  */

/* Dispatch declarations beginning with 'typedef'.  */

static void
typedef_decl (void)
{
  type_p ty, dty;
  const char *name;
  options_p opts;
  bool another;

  gcc_assert (token () == TYPEDEF);
  advance ();

  ty = type (&opts, false);
  if (!ty)
    return;
  if (opts)
    parse_error ("GTY((...)) cannot be applied to a typedef");
  do
    {
      dty = declarator (ty, &name, &opts);
      if (opts)
	parse_error ("GTY((...)) cannot be applied to a typedef");

      /* Yet another place where we could have junk (notably attributes)
	 after the declarator.  */
      another = consume_until_comma_or_semi (false);
      if (dty)
	do_typedef (name, dty, &lexer_line);
    }
  while (another);
}

/* Structure definition: type() does all the work.  */

static void
struct_or_union (void)
{
  options_p dummy;
  type (&dummy, false);
  /* There may be junk after the type: notably, we cannot currently
     distinguish 'struct foo *function(prototype);' from 'struct foo;'
     ...  we could call declarator(), but it's a waste of time at
     present.  Instead, just eat whatever token is currently lookahead
     and go back to lexical skipping mode. */
  advance ();
}

/* GC root declaration:
     (extern|static) gtymarker? type ID array_declarators_opt (';'|'=')
   If the gtymarker is not present, we ignore the rest of the declaration.  */
static void
extern_or_static (void)
{
  options_p opts, opts2, dopts;
  type_p ty, dty;
  const char *name;
  require2 (EXTERN, STATIC);

  if (token () != GTY_TOKEN)
    {
      advance ();
      return;
    }

  opts = gtymarker ();
  ty   = type (&opts2, true);  /* if we get here, it's got a GTY(()) */
  dty  = declarator (ty, &name, &dopts);

  if ((opts && dopts) || (opts && opts2) || (opts2 && dopts))
    parse_error ("GTY((...)) specified more than once for %s", name);
  else if (opts2)
    opts = opts2;
  else if (dopts)
    opts = dopts;

  if (dty)
    {
      note_variable (name, adjust_field_type (dty, opts), opts, &lexer_line);
      require2 (';', '=');
    }
}

/* Definition of a generic VEC structure:

   'DEF_VEC_[IPO]' '(' id ')' ';'

   Scalar VECs require slightly different treatment than otherwise -
   that's handled in note_def_vec, we just pass it along.*/
static void
def_vec (void)
{
  bool is_scalar = (token() == DEFVEC_I);
  const char *type;

  require2 (DEFVEC_OP, DEFVEC_I);
  require ('(');
  type = require2 (ID, SCALAR);
  require (')');
  require (';');

  if (!type)
    return;

  note_def_vec (type, is_scalar, &lexer_line);
  note_def_vec_alloc (type, "none", &lexer_line);
}

/* Definition of an allocation strategy for a VEC structure:

   'DEF_VEC_ALLOC_[IPO]' '(' id ',' id ')' ';'

   For purposes of gengtype, this just declares a wrapper structure.  */
static void
def_vec_alloc (void)
{
  const char *type, *astrat;

  require (DEFVEC_ALLOC);
  require ('(');
  type = require2 (ID, SCALAR);
  require (',');
  astrat = require (ID);
  require (')');
  require (';');

  if (!type || !astrat)
    return;

  note_def_vec_alloc (type, astrat, &lexer_line);
}

/* Parse the file FNAME for GC-relevant declarations and definitions.
   This is the only entry point to this file.  */
void
parse_file (const char *fname)
{
  yybegin (fname);
  for (;;)
    {
      switch (token ())
	{
	case EXTERN:
	case STATIC:
	  extern_or_static ();
	  break;

	case STRUCT:
	case UNION:
	  struct_or_union ();
	  break;

	case TYPEDEF:
	  typedef_decl ();
	  break;

	case DEFVEC_OP:
	case DEFVEC_I:
	  def_vec ();
	  break;

	case DEFVEC_ALLOC:
	  def_vec_alloc ();
	  break;

	case EOF_TOKEN:
	  goto eof;

	default:
	  parse_error ("unexpected top level token, %s", print_cur_token ());
	  goto eof;
	}
      lexer_toplevel_done = 1;
    }

 eof:
  advance ();
  yyend ();
}
