/* { dg-do compile } */
/* { dg-options "-O -fdiagnostics-show-caret" } */

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
   myvar = myvar.x;
           ~~~~~^~
   { dg-end-multiline-output "" } */
#endif
}

void test_simple_2 (void)
{
#if 0
  x = first_function () + second_function ();  /* { dg-warning "test" } */

/* { dg-begin-multiline-output "" }
   x = first_function () + second_function ();
       ~~~~~~~~~~~~~~~~~ ^ ~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
#endif
}


void test_multiline (void)
{
#if 0
  x = (first_function ()
       + second_function ()); /* { dg-warning "test" } */

/* { dg-begin-multiline-output "" }
   x = (first_function ()
        ~~~~~~~~~~~~~~~~~
        + second_function ());
        ^ ~~~~~~~~~~~~~~~~~~
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
   x = (first_function_with_a_very_long_name (lorem, ipsum, dolor, sit, amet,
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                             consectetur, adipiscing, elit,
                                             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                             sed, eiusmod, tempor,
                                             ~~~~~~~~~~~~~~~~~~~~~
                                             incididunt, ut, labore, et,
                                             ~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                             dolore, magna, aliqua)
                                             ~~~~~~~~~~~~~~~~~~~~~~
        + second_function_with_a_very_long_name (lorem, ipsum, dolor, sit,
        ^ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                 amet, consectetur,
                                                 ~~~~~~~~~~~~~~~~~~
                                                 adipiscing, elit, sed,
                                                 ~~~~~~~~~~~~~~~~~~~~~~
                                                 eiusmod, tempor, incididunt,
                                                 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                 ut, labore, et, dolore,
                                                 ~~~~~~~~~~~~~~~~~~~~~~~
                                                 magna, aliqua));
                                                 ~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
#endif
}

void test_richloc_from_proper_range (void)
{
#if 0
  float f = 98.6f; /* { dg-warning "test" } */
/* { dg-begin-multiline-output "" }
   float f = 98.6f;
             ^~~~~
   { dg-end-multiline-output "" } */
#endif
}

void test_caret_within_proper_range (void)
{
#if 0
  float f = foo * bar; /* { dg-warning "17: test" } */
/* { dg-begin-multiline-output "" }
   float f = foo * bar;
             ~~~~^~~~~
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
                                              float f = foo * bar;
                                                        ~~~~^~~~~
   { dg-end-multiline-output "" } */
#endif
}

void test_multiple_carets (void)
{
#if 0
   x = x + y /* { dg-warning "8: test" } */
/* { dg-begin-multiline-output "" }
    x = x + y
        A   B
   { dg-end-multiline-output "" } */
#endif
}

void test_caret_on_leading_whitespace (void)
{
#if 0
    ASSOCIATE (y => x)
      y = 5 /* { dg-warning "6: test" } */
/* { dg-begin-multiline-output "" }
     ASSOCIATE (y => x)
                    2
       y = 5
      1
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
    int a[2][2] = { 0, 1 , 2, 3 };
                    ^~~~
                    {   }
   { dg-end-multiline-output "" } */
#endif
}

/* Unit test for rendering of "remove" fixit hints.  */

void test_fixit_remove (void)
{
#if 0
  int a;; /* { dg-warning "example of a removal hint" } */
/* { dg-begin-multiline-output "" }
   int a;;
         ^
         -
   { dg-end-multiline-output "" } */
#endif
}

/* Unit test for rendering of "replace" fixit hints.  */

void test_fixit_replace (void)
{
#if 0
  gtk_widget_showall (dlg); /* { dg-warning "example of a replacement hint" } */
/* { dg-begin-multiline-output "" }
   gtk_widget_showall (dlg);
   ^~~~~~~~~~~~~~~~~~
   gtk_widget_show_all
   { dg-end-multiline-output "" } */
#endif
}

/* Test of "%q+D" format code.  */

int test_percent_q_plus_d (void)
{
  int local = 0; /* { dg-warning "example of plus in format code" } */
/* { dg-begin-multiline-output "" }
   int local = 0;
       ^~~~~
   { dg-end-multiline-output "" } */
  return local;
}
