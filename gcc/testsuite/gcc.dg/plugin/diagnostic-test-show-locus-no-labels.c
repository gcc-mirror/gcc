/* { dg-do compile } */
/* { dg-options "-O -fdiagnostics-show-caret -fno-diagnostics-show-labels" } */

/* Verify that -fno-diagnostics-show-labels works.  */

/* This is a collection of unittests for diagnostic_show_locus;
   see the overview in diagnostic_plugin_test_show_locus.c.

   In particular, note the discussion of why we need a very long line here:
01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
   and that we can't use macros in this file.  */

void test_multiline (void)
{
#if 0
  x = (first_function ()
       + second_function ()); /* { dg-warning "test" } */

  /* This shouldn't have a label.  */
  /* { dg-begin-multiline-output "" }
   x = (first_function ()
        ~~~~~~~~~~~~~~~~~
        + second_function ());
        ^ ~~~~~~~~~~~~~~~~~~
     { dg-end-multiline-output "" } */
#endif
}
