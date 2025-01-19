/* Test ASM operand modifiers. */

/* { dg-do compile } */
/* { dg-options "-O1" } */

struct S {
  char b;
  int a;
  short c;
};

void
test_const_int (void)
{
  /* { dg-final { scan-assembler "# printing 7 and -5" } } */
  asm volatile ("# printing %c0 and %n1"
    : :
    "i" (sizeof(struct S)),
    "i" (__builtin_offsetof (struct S, c)));
}

extern int g;

void
test_sym (void)
{
  /* { dg-final { scan-assembler "# outputting g and test_sym" } } */
  asm volatile ("# outputting %c0 and %c1"
    : :
    "i" (&g),
    "i" (&test_sym));
}
