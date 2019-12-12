/* { dg-do compile } */
/* Verify "-fdiagnostics-minimum-margin-width=0".  */
/* { dg-options "-O -fdiagnostics-show-caret -fdiagnostics-show-line-numbers -fdiagnostics-minimum-margin-width=0" } */

/* This is a collection of unittests for diagnostic_show_locus;
   see the overview in diagnostic_plugin_test_show_locus.c.

   In particular, note the discussion of why we need a very long line here:
01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
   and that we can't use macros in this file.  */

void test_simple (void)
{
#if 0
  myvar = myvar.x; /* { dg-warning "test" } */

/* { dg-begin-multiline-output "" }
15 |   myvar = myvar.x;
   |           ~~~~~^~
   { dg-end-multiline-output "" } */
#endif
}
