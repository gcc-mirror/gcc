/* { dg-do compile } */
/* { dg-options "-O -fdiagnostics-generate-patch" } */

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
#endif
}

/* Unit test for rendering of "remove" fixit hints.  */

void test_fixit_remove (void)
{
#if 0
  int a;; /* { dg-warning "example of a removal hint" } */
#endif
}

/* Unit test for rendering of "replace" fixit hints.  */

void test_fixit_replace (void)
{
#if 0
  gtk_widget_showall (dlg); /* { dg-warning "example of a replacement hint" } */
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
#endif
}

/* Unit test for mutually-exclusive suggestions.  */

void test_mutually_exclusive_suggestions (void)
{
#if 0
  original; /* { dg-warning "warning 1" } */
/* { dg-warning "warning 2" "" { target *-*-* } .-1 } */
/* We should not print the mutually-incompatible fix-it hints within
   the generated patch; they are not listed in the big expected
   multiline output below.  */
#endif
}

/* Unit tests for add_fixit_insert_formatted.  */

void test_add_fixit_insert_formatted_single_line (void)
{
  {}
}

void test_add_fixit_insert_formatted_multiline (void)
{
  if (1)
    {
    }
}


/* Verify the output from -fdiagnostics-generate-patch.
   We expect a header, containing the filename.  This is the absolute path,
   so we can only capture it via regexps.  */

/* { dg-regexp "\\-\\-\\- .*" } */
/* { dg-regexp "\\+\\+\\+ .*" } */

/* Next, we expect the diff itself.  */
/* { dg-begin-multiline-output "" }
@@ -14,7 +14,7 @@
 void test_fixit_insert (void)
 {
 #if 0
-   int a[2][2] = { 0, 1 , 2, 3 };
+   int a[2][2] = { {0, 1} , 2, 3 };
 #endif
 }
 
@@ -23,7 +23,7 @@
 void test_fixit_remove (void)
 {
 #if 0
-  int a;;
+  int a;
 #endif
 }
 
@@ -32,7 +32,7 @@
 void test_fixit_replace (void)
 {
 #if 0
-  gtk_widget_showall (dlg);
+  gtk_widget_show_all (dlg);
 #endif
 }
 
@@ -45,6 +45,7 @@
     {
     case 'a':
       x = a;
+      break;
     case 'b':
       x = b;
     }
@@ -68,7 +69,7 @@
 
 void test_add_fixit_insert_formatted_single_line (void)
 {
-  {}
+  {INSERTED-CONTENT}
 }
 
 void test_add_fixit_insert_formatted_multiline (void)
@@ -76,6 +77,7 @@
   if (1)
     {
     }
+  INSERTED-CONTENT
 }
 
 
   { dg-end-multiline-output "" } */
