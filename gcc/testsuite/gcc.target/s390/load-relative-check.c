/* Check if load-relative instructions are created */
/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O2 -march=z10 -mzarch -fno-section-anchors" } */

/* { dg-final { scan-assembler "lgfrl\t%r.?,b.4" { target { lp64 } } } } */
/* { dg-final { scan-assembler "lgfrl\t%r.?,s.12" { target { lp64 } } } } */
/* { dg-final { scan-assembler "lgrl\t%r.?,s" { target { lp64 } } } } */

/* { dg-final { scan-assembler "lrl\t%r.?,b.4" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lrl\t%r.?,s.8" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lrl\t%r.?,s" { target { ! lp64 } } } } */

int b[20];

struct s
{
  long a;
  int  b;
  int  c;
} s;

struct __attribute__((packed)) s2
{
  char a;
  char b;
  char c;
} s2;

char __attribute__((aligned(1))) arr[10];

int foo()
  {
    return b[1];
  }

int bar()
  {
    return s.c;
  }

long bar2()
  {
    return s.a;
  }

int baz()
  {
    return arr[1];
  }
