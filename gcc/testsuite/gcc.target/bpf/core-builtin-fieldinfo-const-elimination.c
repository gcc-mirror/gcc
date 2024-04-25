/* { dg-do compile } */
/* { dg-options "-O2 -dA -gbtf -mco-re -masm=normal" } */

struct S {
  unsigned int a1: 7;
  unsigned int a2: 4;
  unsigned int a3: 13;
  unsigned int a4: 5;
  int x;
};

struct T {
  unsigned int y;
  struct S s[2];
  char c;
  char d;
};

enum {
  FIELD_BYTE_OFFSET = 0,
};

unsigned int foo (struct T *t)
{
  return __builtin_preserve_field_info (t->s[0].a1, FIELD_BYTE_OFFSET) + 1;
}

/* { dg-final { scan-assembler-times "\[\t \]lddw\[\t \]%r\[0-9\],4" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]add32\[\t \]%r\[0-9\],1" 1 } } */
