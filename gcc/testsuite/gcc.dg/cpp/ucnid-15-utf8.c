/* Verify macro definitions with UTF-8 in argument names are output in
   -dD output with the original spelling.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c99 -dD" } */
/* { dg-final { scan-file ucnid-15-utf8.i "#define a\\(Á\\) x:Á:y:Á:z" } } */
#define a(Á) x:Á:y:Á:z
