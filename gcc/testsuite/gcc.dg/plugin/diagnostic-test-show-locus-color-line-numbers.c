/* { dg-do compile } */
/* { dg-options "-O -fdiagnostics-show-caret -fplugin-arg-diagnostic_plugin_test_show_locus-color -fdiagnostics-show-line-numbers" } */

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

/* { dg-begin-multiline-output "" }
14 |   x = ([32m[Kfirst_function ()[m[K
   |        [32m[K~~~~~~~~~~~~~~~~~[m[K
15 |        [01;35m[K+[m[K [34m[Ksecond_function ()[m[K);
   |        [01;35m[K^[m[K [34m[K~~~~~~~~~~~~~~~~~~[m[K
   |        [01;35m[K|[m[K
   |        [01;35m[Klabel[m[K
   { dg-end-multiline-output "" } */
#endif
}
