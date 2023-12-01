/* Example of a fix-it hint that adds a #include directive,
   adding them to the top of the file, given that there is no
   pre-existing #include.  */

/* This is padding (to avoid the generated patch containing DejaGnu
   directives).  */

/* { dg-options "-fdiagnostics-generate-patch" } */

void test (int i, int j)
{
  printf ("%i of %i\n", i, j); /* { dg-error "implicit declaration" } */
  /* { dg-message "include '<stdio.h>' or provide a declaration of 'printf'" "" { target *-*-* } .-1 } */
  /* { dg-warning "incompatible implicit declaration of built-in function 'printf'" "" { target *-*-* } .-2 } */
}

/* Verify the output from -fdiagnostics-generate-patch.
   We expect the patch to begin with a header, containing this
   source filename, via an absolute path.
   Given the path, we can only capture it via regexps.  */
/* { dg-regexp "\\-\\-\\- .*" } */
/* { dg-regexp "\\+\\+\\+ .*" } */
/* Use #if 0/#endif rather than comments, to allow the text to contain
   a comment.  */
#if 0
{ dg-begin-multiline-output "" }
@@ -1,3 +1,4 @@
+#include <stdio.h>
 /* Example of a fix-it hint that adds a #include directive,
    adding them to the top of the file, given that there is no
    pre-existing #include.  */
{ dg-end-multiline-output "" }
#endif

/* FIXME: should we attempt to skip leading comments when determining the
   insertion location?
   Similarly, should we attempt to be within single-inclusion guards, etc?  */
