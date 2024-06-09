/* { dg-do run } */
/* { dg-options "-O2 -march=rv32gc -mabi=ilp32d -mtune=sifive-7-series" { target { rv32 } } } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -mtune=sifive-7-series" {target { rv64 } } } */

extern void abort (void);
extern void exit (int);

unsigned short __attribute__ ((noinline, noclone))
foo (unsigned short x) {
  if (x == 1)
    x ^= 0x4002;

  return x;
}

int main () {
  if (foo(1) != 0x4003)
    abort ();

  exit(0);
}
