/* { dg-require-effective-target c++11 } */
/* { dg-options "--coverage -std=c++11" } */
/* { dg-do run } */

void lambda1 ()
{
  /* From pr86109.C  */
  auto partially_uncovered_lambda1 = [](int i) { /* count(1) */
#pragma GCC suppress_coverage begin
    if (i > 10) /* count(#) */
      return 0; /* count(#) */
#pragma GCC suppress_coverage end
    return 1;   /* count(#####) */
  };

  auto partially_uncovered_lambda2 = [](int i) { /* count(1) */
    if (i > 10) /* count(1) */
#pragma GCC suppress_coverage begin
      return 0; /* count(#) */
#pragma GCC suppress_coverage end
    return 1;   /* count(#####) */
  };

  partially_uncovered_lambda1 (20);
  partially_uncovered_lambda2 (20);
}

void lambda2 ()
{
  /* From pr86109.C  */
#pragma GCC suppress_coverage begin
  auto partially_uncovered_lambda1 = [](int i) { /* count(#) */
    if (i > 10) /* count(#) */
      return 0; /* count(#) */
#pragma GCC suppress_coverage end
    return 1; /* count(#####) */
  };

  auto partially_uncovered_lambda2 = [](int i) { /* count(1) */
    if (i > 10) /* count(1) */
#pragma GCC suppress_coverage begin
      return 0; /* count(#) */
#pragma GCC suppress_coverage end
    return 1; /* count(#####) */
  };

#pragma GCC suppress_coverage begin
  auto partially_uncovered_lambda3 = [](int i) { /* count(#) */
#pragma GCC suppress_coverage end
    if (i > 10) /* count(1) */
      return 0; /* count(1) */
    return 1; /* count(#####) */
  };

  partially_uncovered_lambda1 (20);
  partially_uncovered_lambda2 (20);
  partially_uncovered_lambda3 (20);
}

void lambda3 ()
{
#pragma GCC suppress_coverage begin
  auto fully_covered_lambda1 = [](int i) { /* count(#) */
    if (i > 10) /* count(#) */
      return 0; /* count(#) */
    return 1;	/* count(#) */
  };
#pragma GCC suppress_coverage end

  fully_covered_lambda1 (20);
}

int main ()
{
  lambda1 ();
  lambda2 ();
  lambda3 ();
}

/* { dg-final { run-gcov gcov-27.C } } */
