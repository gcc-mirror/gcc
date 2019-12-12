/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

int a=0;

int main() {
  for (;; a++) {
    int c[1];
    if (a) {
      break;
      a;
      continue; /* count(1) */
    }
    continue; /* count(1) */
  }

  return 0;
}

/* { dg-final { run-gcov gcov-pr85217.c } } */
