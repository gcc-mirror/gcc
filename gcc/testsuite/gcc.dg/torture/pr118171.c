/* { dg-do compile } */

_Complex float f;
static int d;

void
foo(char *p)
{
  do {
    __builtin_memcpy(&d, 3 + p, 2);
  } while (*(_Complex char *)&f);
}
