/* { dg-do compile } */
/* { dg-options "-O2 -fwritable-strings -w" } */
/* { dg-error "-fwritable-strings is deprecated" "" { target *-*-* } 0 } */

struct S { char *a, *b; };

int
main ()
{
  struct S s[] = {
    {"ABCDEFGH0123", "T"},
    {"ABCDEFGH4567", "T"},
    {"ABCDEFGH89ZYX", "T"},
    {"IJK012", "T"},
    {"IJK345", "T"},
    {"IJK678", "T"},
    {"IJKLMN", "T"},
    {"IJKOPQ", "T"},
    {0, 0}
  };

  __asm __volatile ("" : : "r" (s) : "memory");
  return 0;
}

/* Test whether strings aren't output more than once.  */
/* { dg-final { scan-assembler-not "ABCDEFGH0123.*ABCDEFGH0123" } } */
