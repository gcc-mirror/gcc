/* PR target/82975.  */
/* { dg-do compile } */
/* { dg-options "-fno-sched-pressure -O2" } */
/* { dg-additional-options "-mtune=cortex-a57" { target arm*-*-* aarch64*-*-* } } */

typedef __SIZE_TYPE__ size_t;

struct S1
{
  char pad1;
  char val;
  short pad2;
};

extern char t[256];

void foo (struct S1 a, size_t i)
{
  t[i] = a.val;
}
