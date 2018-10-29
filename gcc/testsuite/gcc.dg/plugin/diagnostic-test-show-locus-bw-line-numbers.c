/* { dg-do compile } */
/* { dg-options "-O -fdiagnostics-show-caret -fdiagnostics-show-line-numbers" } */

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
   14 |   myvar = myvar.x;
      |           ~~~~~^~
   { dg-end-multiline-output "" } */
#endif
}

void test_multiline (void)
{
#if 0
  x = (first_function ()
       + second_function ()); /* { dg-warning "test" } */

/* { dg-begin-multiline-output "" }
   26 |   x = (first_function ()
      |        ~~~~~~~~~~~~~~~~~
   27 |        + second_function ());
      |        ^ ~~~~~~~~~~~~~~~~~~
      |        |
      |        label
   { dg-end-multiline-output "" } */
#endif
}

void test_very_wide_line (void)
{
#if 0
                                                                                float f = foo * bar; /* { dg-warning "95: test" } */
/* { dg-begin-multiline-output "" }
      |        0         0         0         0         0         1         1  
      |        5         6         7         8         9         0         1  
      | 3456789012345678901234567890123456789012345678901234567890123456789012
   43 |                                       float f = foo * bar;
      |                                                 ~~~~^~~~~
      |                                                     |
      |                                                     label 0
      |                                                 bar * foo
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
   63 |    int a[2][2] = { 0, 1 , 2, 3 };
      |                    ^~~~
      |                    {   }
   { dg-end-multiline-output "" } */
#endif
}

/* Unit test for rendering of "remove" fixit hints.  */

void test_fixit_remove (void)
{
#if 0
  int a;; /* { dg-warning "example of a removal hint" } */
/* { dg-begin-multiline-output "" }
   77 |   int a;;
      |         ^
      |         -
   { dg-end-multiline-output "" } */
#endif
}

/* Unit test for rendering of "replace" fixit hints.  */

void test_fixit_replace (void)
{
#if 0
  gtk_widget_showall (dlg); /* { dg-warning "example of a replacement hint" } */
/* { dg-begin-multiline-output "" }
   91 |   gtk_widget_showall (dlg);
      |   ^~~~~~~~~~~~~~~~~~
      |   gtk_widget_show_all
   { dg-end-multiline-output "" } */
#endif
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
  109 |       x = a;
  +++ |+      break;
  110 |     case 'b':
      |     ^~~~~~~~
   { dg-end-multiline-output "" } */
#endif
}
