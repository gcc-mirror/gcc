/* Check -Wbuiltin-declaration-mismatch can be ignored with pragma.  */
/* { dg-do compile }
   { dg-options "-Wno-implicit-function-declaration -Wno-int-conversion -Wbuiltin-declaration-mismatch" } */

#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
int foo (const char *str)
{
  int i;
  sscanf (str, "%d", &i);
  return i;
}
