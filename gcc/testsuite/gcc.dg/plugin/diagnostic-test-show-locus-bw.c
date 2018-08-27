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
        |
        label
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
        |
        label 1
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
        | |
        | label 2
        label 0
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
                             float x                                                    = foo * bar; /* { dg-warning "95: test" } */
/* { dg-begin-multiline-output "" }
     0         0         0         0         0         0         1     
     4         5         6         7         8         9         0     
 6789012345678901234567890123456789012345678901234567890123456789012345
 x                                                    = foo * bar;
 ~                                                      ~~~~^~~~~
 |                                                          |
 label 1                                                    label 0
                                                        bar * foo
   { dg-end-multiline-output "" } */
#endif
}

void test_very_wide_line_2 (void)
{
#if 0
                            float x                                                     = foo * bar; /* { dg-warning "95: test" } */
/* { dg-begin-multiline-output "" }
     0         0         0         0         0         0         1     
     4         5         6         7         8         9         0     
 6789012345678901234567890123456789012345678901234567890123456789012345
                                                      = foo * bar;
                                                        ~~~~^~~~~
                                                            |
                                                            label 0
                                                        bar * foo
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

/* Test of many nested locations and fixits.  */

void test_many_nested_locations (void)
{
  /* { dg-warning "test of 70 locations" }
    Lorem ipsum dolor sit amet, consectetur adipiscing elit,
    sed do eiusmod tempor incididunt ut labore et dolore magna
    aliqua. Ut enim ad minim veniam, quis nostrud exercitation
    ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis
    aute irure dolor in reprehenderit in voluptate velit esse cillum
    dolore eu fugiat nulla pariatur. Excepteur sint occaecat
    cupidatat non proident, sunt in culpa qui officia deserunt
    mollit anim id est laborum.
  */
/* { dg-begin-multiline-output "" }
   /*
   ^
     Lorem ipsum dolor sit amet, consectetur adipiscing elit,
     ^~~~~ ^~~~~ ^~~~~ ^~~ ^~~~  ^~~~~~~~~~~ ^~~~~~~~~~ ^~~~
     |     |     |     |   |     |           |          |
     |     |     |     |   label label       label      label
     label label label label
     LOREM IPSUM DOLOR SIT AMET  CONSECTETUR ADIPISCING ELIT
     sed do eiusmod tempor incididunt ut labore et dolore magna
     ^~~ ^~ ^~~~~~~ ^~~~~~ ^~~~~~~~~~ ^~ ^~~~~~ ^~ ^~~~~~ ^~~~~
     |   |  |       |      |          |  |      |  |      |
     |   |  |       |      |          |  |      |  label  label
     |   |  |       |      |          |  label  label
     |   |  label   label  label      label
     |   label
     label
     SED DO EIUSMOD TEMPOR INCIDIDUNT UT LABORE ET DOLORE MAGNA
     aliqua. Ut enim ad minim veniam, quis nostrud exercitation
     ^~~~~~  ^~ ^~~~ ^~ ^~~~~ ^~~~~~  ^~~~ ^~~~~~~ ^~~~~~~~~~~~
     |       |  |    |  |     |       |    |       |
     |       |  |    |  |     |       |    label   label
     |       |  |    |  label label   label
     |       |  |    label
     |       |  label
     label   label
     ALIQUA  UT ENIM AD MINIM VENIAM  QUIS NOSTRUD EXERCITATION
     ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis
     ^~~~~~~ ^~~~~~~ ^~~~ ^~ ^~~~~~~ ^~ ^~ ^~~~~~~ ^~~~~~~~~  ^~~~
     |       |       |    |  |       |  |  |       |          |
     |       |       |    |  |       |  |  label   label      label
     |       |       |    |  |       |  label
     |       |       |    |  label   label
     |       |       |    label
     label   label   label
     ULLAMCO LABORIS NISI UT ALIQUIP EX EA COMMODO CONSEQUAT  DUIS
     aute irure dolor in reprehenderit in voluptate velit esse cillum
     ^~~~ ^~~~~ ^~~~~ ^~ ^~~~~~~~~~~~~ ^~ ^~~~~~~~~ ^~~~~ ^~~~ ^~~~~~
     |    |     |     |  |             |  |         |     |    |
     |    |     |     |  |             |  |         |     |    label
     |    |     |     |  |             |  label     label label
     |    |     |     |  label         label
     |    label label label
     label
     AUTE IRURE DOLOR IN REPREHENDERIT IN VOLUPTATE VELIT ESSE CILLUM
     dolore eu fugiat nulla pariatur. Excepteur sint occaecat
     ^~~~~~ ^~ ^~~~~~ ^~~~~ ^~~~~~~~  ^~~~~~~~~ ^~~~ ^~~~~~~~
     |      |  |      |     |         |         |    |
     |      |  |      |     |         |         |    label
     |      |  label  label label     label     label
     label  label
     DOLORE EU FUGIAT NULLA PARIATUR  EXCEPTEUR SINT OCCAECAT
     cupidatat non proident, sunt in culpa qui officia deserunt
     ^~~~~~~~~ ^~~ ^~~~~~~~  ^~~~ ^~ ^~~~~ ^~~ ^~~~~~~ ^~~~~~~~
     |         |   |         |    |  |     |   |       |
     |         |   |         |    |  |     |   label   label
     |         |   |         |    |  label label
     |         |   |         |    label
     |         |   label     label
     label     label
     CUPIDATAT NON PROIDENT  SUNT IN CULPA QUI OFFICIA DESERUNT
     mollit anim id est laborum.
     ^~~~~~ ^~~~ ^~ ^~~ ^~~~~~~
     |      |    |  |   |
     |      |    |  |   label
     |      |    |  label
     |      |    label
     label  label
     MOLLIT ANIM ID EST LABORUM
   { dg-end-multiline-output "" } */
}

/* Unit test for rendering of fix-it hints that add new lines.  */

void test_fixit_insert_newline (void)
{
#if 0
  switch (op)
    {
    case 'a':
      x = a;
    case 'b':  /* { dg-warning "newline insertion" } */
      x = b;
    }
/* { dg-begin-multiline-output "" }
       x = a;
+      break;
     case 'b':
     ^~~~~~~~
   { dg-end-multiline-output "" } */
#endif
}

/* Unit test for mutually-exclusive suggestions.  */

void test_mutually_exclusive_suggestions (void)
{
#if 0
  original; /* { dg-warning "warning 1" } */
/* { dg-warning "warning 2" "" { target *-*-* } .-1 } */
/* { dg-begin-multiline-output "" }
   original;
   ^~~~~~~~
   replacement_1
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
   original;
   ^~~~~~~~
   replacement_2
   { dg-end-multiline-output "" } */
#endif
}
