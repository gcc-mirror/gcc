/* PR target/88809 */
/* { dg-options "-Os" } */

unsigned int foo (const char *ptr)
{
  return __builtin_strlen (ptr);
}

/* { dg-final { scan-assembler "call\[ \t\]strlen" } } */
