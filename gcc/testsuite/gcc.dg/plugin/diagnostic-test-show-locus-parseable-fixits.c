/* { dg-do compile } */
/* { dg-options "-O -fdiagnostics-parseable-fixits" } */

/* This is a collection of unittests for diagnostic_show_locus;
   see the overview in diagnostic_plugin_test_show_locus.c.

   In particular, note the discussion of why we need a very long line here:
01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
   and that we can't use macros in this file.  */

/* Unit test for rendering of insertion fixit hints
   (example taken from PR 62316).  */

void test_fixit_insert (void)
{
#if 0
   int a[2][2] = { 0, 1 , 2, 3 }; /* { dg-warning "insertion hints" } */
/* { dg-regexp "fix-it:.*\\{17:20-17:20\\}:.*" } */
/* { dg-regexp "fix-it:.*\\{17:24-17:24\\}:.*" } */
#endif
}

/* Unit test for rendering of "remove" fixit hints.  */

void test_fixit_remove (void)
{
#if 0
  int a;; /* { dg-warning "example of a removal hint" } */
/* { dg-regexp "fix-it:.*\\{28:9-28:10\\}:.*" } */
#endif
}

/* Unit test for rendering of "replace" fixit hints.  */

void test_fixit_replace (void)
{
#if 0
  gtk_widget_showall (dlg); /* { dg-warning "example of a replacement hint" } */
/* { dg-regexp "fix-it:.*\\{38:3-38:21\\}:.*" } */
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
    case 'b': /* { dg-warning "newline insertion" } */
      x = b;
    }
/* { dg-regexp "fix-it:.*\\{52:1-52:1\\}:.*\\n" } */
#endif
}

/* Unit test for mutually-exclusive suggestions.  */

void test_mutually_exclusive_suggestions (void)
{
#if 0
  original; /* { dg-warning "warning 1" } */
/* { dg-warning "warning 2" "" { target *-*-* } .-1 } */
/* We should print the mutually-incompatible fix-it hints within
   -fdiagnostics-parseable-fixits; verify that they are printed.  */
/* { dg-regexp "fix-it:.*\\{64:3-64:11}:.*\\n" } */
/* { dg-regexp "fix-it:.*\\{64:3-64:11}:.*\\n" } */
#endif
}
