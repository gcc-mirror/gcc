/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

int main()
{
  int a = 1;
  int b = 2;
  int c = -3;
  switch(a) /* count(1) */
    {
    case 1: /* count(1) */
    c = 3;
    switch(b) { /* count(1) */
      case 1: /* count(#####) */
      c = 4;
      break;
      case 2: /* count(1) */
      c = 5;
      break;
    }
    break;
    case 2: /* count(#####) */
    c = 6;
    break;
    default: /* count(#####) */
    break;
    }
}

/* { dg-final { run-gcov gcov-24.c } } */
