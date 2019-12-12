/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */
/* { dg-require-effective-target indirect_jumps } */

void *buf[5];

void fjmp (void) {
  __builtin_longjmp (buf, 1);
}

int main(void)
{
  int last = 0;

  if (__builtin_setjmp (buf) == 0) {	/* count(2) */
    __builtin_printf("True  branch\n");
    while (1) {
      last = 1;	/* count(1) */
      fjmp ();	/* count(1) */
    }
  } else {
    __builtin_printf("False branch\n");
  }

  return 0;
}

/* { dg-final { run-gcov gcov-pr85372.c } } */
