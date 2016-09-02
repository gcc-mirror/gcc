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
 
   { dg-end-multiline-output "" } */
