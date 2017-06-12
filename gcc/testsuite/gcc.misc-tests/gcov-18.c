/* Test gcov block mode. As the example does jump to a statement
   that is on a line with different ones, we must calculate such jump
   just once.  */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

int a = 0;

void foo() /* count(1) */
{
  a = 1;
}

void bar() /* count(1) */
{
  a++;
}

int main() /* count(1) */
{
  foo (); goto baz; lab: bar (); /* count(2) */

  baz:
    if (a == 1) /* count(2) */
      goto lab; /* count(1) */
}

/* { dg-final { run-gcov { gcov-18.c } } } */
