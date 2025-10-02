/* { dg-do compile } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("default"))) void
foo (int a = 3);

__attribute__ ((target_version ("sve"))) void
foo (int a = 4);

void bar() {
  foo();
}

__attribute__ ((target_version ("sve"))) void
foo2 (int a = 6);

__attribute__ ((target_version ("default"))) void
foo2 (int a = 5);

void bar2() {
  foo2();
}


/* { dg-final { scan-assembler-times "\n\tmov\tw\[0-9\]\+, 3\n" 1 } } */
/* { dg-final { scan-assembler-times "\n\tmov\tw\[0-9\]\+, 5\n" 1 } } */
