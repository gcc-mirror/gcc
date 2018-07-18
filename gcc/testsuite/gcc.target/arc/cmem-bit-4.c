/* { dg-do compile } */
/* { dg-options "-mcpu=nps400 -mcmem -O2" } */

struct strange_bool
{
  unsigned long long bool_bit   :1;
  unsigned long long other_bits :61;
};

struct strange_bool a_strange_bool __attribute__((section(".cmem")));

extern void bar();

void foo() {
  if (a_strange_bool.bool_bit)
    bar();
}

/* { dg-final { scan-assembler "xldb r\[0-9\]+,\\\[@a_strange_bool\\\]" } } */
/* { dg-final { scan-assembler "btst_s r\[0-9\]+,7" { target arceb-*-* } } } */
