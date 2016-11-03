// PR gcov-profile/64634
// { dg-options "-fprofile-arcs -ftest-coverage" }
// { dg-do run { target native } }

int main()
{
  return 0;   /* count(#####) */
}

// { dg-final { run-gcov remove-gcda gcov-16.C } }
