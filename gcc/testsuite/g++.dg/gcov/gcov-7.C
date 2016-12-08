/* Check that Exception handler specification is not
   mapped to the curly braces below the function
   declaration.  */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

struct foo
{
  foo ()
#if __cplusplus <= 201402L
    throw (int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
    {			/* count (-) */
      throw (1);
    }
};

int main ()
{
  try
    {
      foo f;
    }
  catch ( ...)
    {
      return 0;
    }
}

/* { dg-final { run-gcov gcov-7.C } } */
