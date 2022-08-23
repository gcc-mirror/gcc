/* { dg-do run } */
/* { dg-options "" } */

extern void abort (void);
extern void exit (int);

unsigned int __attribute__ ((noinline, noclone))
foo(unsigned int i) {

  return 0xFFFF & (0xd066 << (((i & 0x1) ^ 0x2f) & 0xf));
}

int main() {
  if (foo (1) != 0x8000)
    abort ();
  exit (0);
}
