/* Example of where the error occurs before the first #include,
   which in this case happens to be the missing header. 
   For this case, expect to insert the #include at the top of the file. */

/* { dg-options "-fdiagnostics-generate-patch" } */
/* { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } } */

void test ()
{
  std::string test; // { dg-error ".string. is not a member of .std." }
  // { dg-message ".std::string. is defined in header .<string>.; this is probably fixable by adding .#include <string>." "" { target *-*-* } .-1 }
}

#include <string>

/* Verify the output from -fdiagnostics-generate-patch.
   We expect the patch to begin with a header, containing this
   source filename, via an absolute path.
   Given the path, we can only capture it via regexps.  */
/* { dg-regexp "\\-\\-\\- .*" } */
/* { dg-regexp "\\+\\+\\+ .*" } */

/* Verify the hunks within the patch.
   Use #if 0/#endif rather than comments, to allow the text to contain
   a comment.
   We expect a "#include <string>" have been added once, at the top
   of the file.  */
#if 0
{ dg-begin-multiline-output "" }
@@ -1,3 +1,4 @@
+#include <string>
 /* Example of where the error occurs before the first #include,
    which in this case happens to be the missing header. 
    For this case, expect to insert the #include at the top of the file. */
{ dg-end-multiline-output "" }
#endif
