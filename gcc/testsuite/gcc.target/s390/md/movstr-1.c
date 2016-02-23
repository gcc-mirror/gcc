/* Machine description pattern tests.  */

/* { dg-do compile } */
/* { dg-options "-dP" } */

__attribute__ ((noinline))
void test(char *dest, const char *src)
{
  __builtin_stpcpy (dest, src);
}

/* { dg-final { scan-assembler-times {{[*]movstr}|{vec_vfenesv16qi}} 1 } } */
