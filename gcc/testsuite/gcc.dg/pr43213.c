/* { dg-do compile } */

struct S {
  int i;
};

struct S f(int i)
{
  return *(struct S *)&i;
}

/* { dg-final { scan-assembler-not "memmove" } } */
