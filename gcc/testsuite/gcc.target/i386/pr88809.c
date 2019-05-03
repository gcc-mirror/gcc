/* PR target/88809 */
/* { dg-options "-O" } */

unsigned int foo (const char *ptr)
{
  return __builtin_strlen (ptr);
}

/* { dg-final { scan-assembler "call\[ \t\]_?strlen" } } */
