/* { dg-do assemble } */
/* { dg-options "--param asan-instrument-writes=0 -save-temps" } */

volatile int ten = 10;

int main() {
  volatile char x[10];
  x[ten] = 1;
  return 0;
}

/* { dg-final { scan-assembler-not "__asan_store" } } */
/* { dg-final { cleanup-saved-temps } } */
