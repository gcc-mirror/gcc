/* This is padding (to avoid the generated patch containing DejaGnu
   directives).  */

/* { dg-options "-fdiagnostics-generate-patch" } */

void test()
{
  typeid(void); // { dg-error "must '#include <typeinfo>' before using 'typeid'" }
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
+#include <typeinfo>
 /* This is padding (to avoid the generated patch containing DejaGnu
    directives).  */
 
{ dg-end-multiline-output "" }
#endif
