/* Basic tests of the #assert preprocessor extension. */
/* { dg-do compile } */
/* { dg-options "-traditional-cpp" } */

/* #define def unused expansion */ /* tradcpp can't handle macros in answers.  */
#define fail int failit

#assert abc (def)
#assert abc (ghi)
#assert abc (jkl)
#assert space ( s p a c e )

/* Basic: */
#if !#abc (def) || !#abc (ghi) || !#abc (jkl)
fail
#endif

/* any answer for #abc */
#if !#abc
fail
#endif

/* internal whitespace is collapsed,
   external whitespace is deleted  */
#if !#space (s p  a  c e) || !#space (  s p a c e  ) || #space (space)
fail
#endif

/* removing assertions */
#unassert abc (jkl)
#if !#abc || !#abc (def) || !#abc (ghi) || #abc (jkl)
fail
#endif

#unassert abc
#if #abc || #abc (def) || #abc (ghi) || #abc (jkl)
fail
#endif

int gobble

/* make sure it can succeed too.
   also check space before open paren isn't significant */
#if #space(s p a c e)
;
#endif
