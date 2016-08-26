/* { dg-do compile } */
/* { dg-options "-O -fdiagnostics-show-caret -fplugin-arg-diagnostic_plugin_test_show_locus-color" } */

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
   myvar = [32m[Kmyvar[m[K[01;35m[K.[m[K[34m[Kx[m[K;
           [32m[K~~~~~[m[K[01;35m[K^[m[K[34m[K~[m[K
   { dg-end-multiline-output "" } */
#endif
}

void test_simple_2 (void)
{
#if 0
  x = first_function () + second_function ();  /* { dg-warning "test" } */

/* { dg-begin-multiline-output "" }
   x = [32m[Kfirst_function ()[m[K [01;35m[K+[m[K [34m[Ksecond_function ()[m[K;
       [32m[K~~~~~~~~~~~~~~~~~[m[K [01;35m[K^[m[K [34m[K~~~~~~~~~~~~~~~~~~[m[K
   { dg-end-multiline-output "" } */
#endif
}


void test_multiline (void)
{
#if 0
  x = (first_function ()
       + second_function ()); /* { dg-warning "test" } */

/* { dg-begin-multiline-output "" }
   x = ([32m[Kfirst_function ()[m[K
        [32m[K~~~~~~~~~~~~~~~~~[m[K
        [01;35m[K+[m[K [34m[Ksecond_function ()[m[K);
        [01;35m[K^[m[K [34m[K~~~~~~~~~~~~~~~~~~[m[K
   { dg-end-multiline-output "" } */
#endif
}

void test_many_lines (void)
{
#if 0
  x = (first_function_with_a_very_long_name (lorem, ipsum, dolor, sit, amet,
                                            consectetur, adipiscing, elit,
                                            sed, eiusmod, tempor,
                                            incididunt, ut, labore, et,
                                            dolore, magna, aliqua)
       + second_function_with_a_very_long_name (lorem, ipsum, dolor, sit, /* { dg-warning "test" } */
                                                amet, consectetur,
                                                adipiscing, elit, sed,
                                                eiusmod, tempor, incididunt,
                                                ut, labore, et, dolore,
                                                magna, aliqua));

/* { dg-begin-multiline-output "" }
   x = ([32m[Kfirst_function_with_a_very_long_name (lorem, ipsum, dolor, sit, amet,[m[K
        [32m[K~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~[m[K
 [32m[K                                            consectetur, adipiscing, elit,[m[K
                                             [32m[K~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~[m[K
 [32m[K                                            sed, eiusmod, tempor,[m[K
                                             [32m[K~~~~~~~~~~~~~~~~~~~~~[m[K
 [32m[K                                            incididunt, ut, labore, et,[m[K
                                             [32m[K~~~~~~~~~~~~~~~~~~~~~~~~~~~[m[K
 [32m[K                                            dolore, magna, aliqua)[m[K
                                             [32m[K~~~~~~~~~~~~~~~~~~~~~~[m[K
        [01;35m[K+[m[K [34m[Ksecond_function_with_a_very_long_name (lorem, ipsum, dolor, sit,
        [01;35m[K^[m[K [34m[K~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~[m[K
 [34m[K                                                amet, consectetur,[m[K
                                                 [34m[K~~~~~~~~~~~~~~~~~~[m[K
 [34m[K                                                adipiscing, elit, sed,[m[K
                                                 [34m[K~~~~~~~~~~~~~~~~~~~~~~[m[K
 [34m[K                                                eiusmod, tempor, incididunt,[m[K
                                                 [34m[K~~~~~~~~~~~~~~~~~~~~~~~~~~~~[m[K
 [34m[K                                                ut, labore, et, dolore,[m[K
                                                 [34m[K~~~~~~~~~~~~~~~~~~~~~~~[m[K
 [34m[K                                                magna, aliqua)[m[K);
                                                 [34m[K~~~~~~~~~~~~~~[m[K
   { dg-end-multiline-output "" } */
#endif
}

void test_richloc_from_proper_range (void)
{
#if 0
  float f = 98.6f; /* { dg-warning "test" } */
/* { dg-begin-multiline-output "" }
   float f = [01;35m[K98.6f[m[K;
             [01;35m[K^~~~~[m[K
   { dg-end-multiline-output "" } */
#endif
}

void test_caret_within_proper_range (void)
{
#if 0
  float f = foo * bar; /* { dg-warning "17: test" } */
/* { dg-begin-multiline-output "" }
   float f = [01;35m[Kfoo * bar[m[K;
             [01;35m[K~~~~^~~~~[m[K
   { dg-end-multiline-output "" } */
#endif
}

void test_very_wide_line (void)
{
#if 0
                                                                                float f = foo * bar; /* { dg-warning "95: test" } */
/* { dg-begin-multiline-output "" }
     0         0         0         0         0         0         1     
     4         5         6         7         8         9         0     
 6789012345678901234567890123456789012345678901234567890123456789012345
                                              float f = [01;35m[Kfoo * bar[m[K;
                                                        [01;35m[K~~~~^~~~~[m[K
   { dg-end-multiline-output "" } */
#endif
}

void test_multiple_carets (void)
{
#if 0
   x = x + y /* { dg-warning "8: test" } */
/* { dg-begin-multiline-output "" }
    x = [01;35m[Kx[m[K + [32m[Ky[m[K
        [01;35m[KA[m[K   [32m[KB[m[K
   { dg-end-multiline-output "" } */
#endif
}

void test_caret_on_leading_whitespace (void)
{
#if 0
    ASSOCIATE (y => x)
      y = 5 /* { dg-warning "6: test" } */
/* { dg-begin-multiline-output "" }
     ASSOCIATE (y =>[32m[K [m[Kx)
                    [32m[K2[m[K
      [01;35m[K [m[Ky = 5
      [01;35m[K1[m[K
   { dg-end-multiline-output "" } */
#endif
}

/* Unit test for rendering of insertion fixit hints
   (example taken from PR 62316).  */

void test_fixit_insert (void)
{
#if 0
   int a[2][2] = { 0, 1 , 2, 3 }; /* { dg-warning "insertion hints" } */
/* { dg-begin-multiline-output "" }
    int a[2][2] = { [01;35m[K0, 1[m[K , 2, 3 };
                    [01;35m[K^~~~[m[K
                    [32m[K{[m[K   [32m[K}[m[K
   { dg-end-multiline-output "" } */
#endif
}

/* Unit test for rendering of "remove" fixit hints.  */

void test_fixit_remove (void)
{
#if 0
  int a;; /* { dg-warning "example of a removal hint" } */
/* { dg-begin-multiline-output "" }
   int a;[01;35m[K;[m[K
         [01;35m[K^[m[K
         [31m[K-[m[K
   { dg-end-multiline-output "" } */
#endif
}

/* Unit test for rendering of "replace" fixit hints.  */

void test_fixit_replace (void)
{
#if 0
  gtk_widget_showall (dlg); /* { dg-warning "example of a replacement hint" } */
/* { dg-begin-multiline-output "" }
   [01;35m[Kgtk_widget_showall[m[K (dlg);
   [01;35m[K^~~~~~~~~~~~~~~~~~[m[K
   [32m[Kgtk_widget_show_all[m[K
   { dg-end-multiline-output "" } */
#endif
}
