/* Verify that when we suggest adding #include directives that they
   are added to the affected file.  */

/* The following header file is missing a "#include <stdio.h>".  */

#include "missing-header-fixit-2.h"

/* These directives actually apply to the header.  */
/* { dg-warning "implicit declaration of function 'printf'" "" { target *-*-* } 6 } */
/* { dg-warning "incompatible implicit declaration of built-in function 'printf'" "" { target *-*-* } 6 } */

/* { dg-options "-fdiagnostics-generate-patch" } */

/* Verify the output from -fdiagnostics-generate-patch.
   We expect the patch to begin with a header, containing the
   filename of the header, via an absolute path.
   Given the path, we can only capture it via regexps.  */
/* { dg-regexp "\\-\\-\\- .*" } */
/* { dg-regexp "\\+\\+\\+ .*" } */
/* Use #if 0/#endif rather than comments, to allow the text to contain
   a comment.
   We expect the *header* to have been patched, adding the missing include.  */
#if 0
{ dg-begin-multiline-output "" }
@@ -1,3 +1,4 @@
+#include <stdio.h>
 /* This is missing-header-fixit-2.h, for use by
    missing-header-fixit-2.c  */
 
{ dg-end-multiline-output "" }
#endif
