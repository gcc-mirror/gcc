// { dg-options "-fprofile-arcs -ftest-coverage" }
// { dg-do run { target native } }

int b, c, d, e;

int main()
{
  int a = b < 1 ? (c < 3 ? d : c) : e;	/* count(1*) */
  return a;
}

// { dg-final { run-gcov ternary.C } }
