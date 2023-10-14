/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file" } */

# 0 "this-file-does-not-exist.c"
#warning message

/* Verify that some JSON was written to a file with the expected name.  */
/* { dg-final { verify-sarif-file } } */

/* ...and that it at least includes the warning
       { dg-final { scan-sarif-file "\"message\": " } }
         { dg-final { scan-sarif-file "\"text\": \"#warning message" } } */
