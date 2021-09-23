#pragma omp error asdf				/* { dg-error "expected 'at', 'severity' or 'message' clause" } */
#pragma omp error at				/* { dg-error "expected '\\\(' before end of line" } */
#pragma omp error at(				/* { dg-error "expected 'execution' or 'compilation'" } */
						/* { dg-error "expected '\\\)' before end of line" "" { target *-*-* } .-1 } */
#pragma omp error at(runtime)			/* { dg-error "expected 'execution' or 'compilation'" } */
#pragma omp error at(+				/* { dg-error "expected 'execution' or 'compilation'" } */
						/* { dg-error "expected '\\\)' before '\\\+' token" "" { target *-*-* } .-1 } */
#pragma omp error at(compilation		/* { dg-error "expected '\\\)' before end of line" } */
						/* { dg-error "'pragma omp error' encountered" "" { target *-*-* } .-1 } */
#pragma omp error severity			/* { dg-error "expected '\\\(' before end of line" } */
#pragma omp error severity(			/* { dg-error "expected 'warning' or 'fatal'" } */
						/* { dg-error "expected '\\\)' before end of line" "" { target *-*-* } .-1 } */
#pragma omp error severity(error)		/* { dg-error "expected 'warning' or 'fatal'" } */
#pragma omp error severity(-			/* { dg-error "expected 'warning' or 'fatal'" } */
						/* { dg-error "expected '\\\)' before '-' token" "" { target *-*-* } .-1 } */
#pragma omp error severity(fatal		/* { dg-error "expected '\\\)' before end of line" } */
						/* { dg-error "'pragma omp error' encountered" "" { target *-*-* } .-1 } */
#pragma omp error message			/* { dg-error "expected '\\\(' before end of line" } */
#pragma omp error message(			/* { dg-error "expected expression before end of line" "" { target c } } */
						/* { dg-error "expected primary-expression before end of line" "" { target c++ } .-1 } */
						/* { dg-error "expected '\\\)' before end of line" "" { target c++ } .-2 } */
						/* { dg-error "'pragma omp error' encountered: <message unknown at compile time>" "" { target *-*-* } .-3 } */
#pragma omp error message(0			/* { dg-error "expected '\\\)' before end of line" } */
						/* { dg-error "'pragma omp error' encountered: <message unknown at compile time>" "" { target *-*-* } .-1 } */
#pragma omp error message("foo"			/* { dg-error "expected '\\\)' before end of line" } */
						/* { dg-error "'pragma omp error' encountered: foo" "" { target *-*-* } .-1 } */
#pragma omp error message(1)			/* { dg-error "'pragma omp error' encountered: <message unknown at compile time>" } */
						/* { dg-error "invalid conversion from 'int' to 'const char\\*'" "" { target c++ } .-1 } */
#pragma omp error message(1.2)			/* { dg-error "cannot convert to a pointer type" "" { target c } } */
						/* { dg-error "could not convert" "" { target c++ } .-1 } */
						/* { dg-error "'pragma omp error' encountered: <message unknown at compile time>" "" { target *-*-* } .-2 } */
#pragma omp error message(L"bar")		/* { dg-error "'pragma omp error' encountered: <message unknown at compile time>" } */
						/* { dg-error "could not convert" "" { target c++ } .-1 } */
#pragma omp error message("foo"),at(compilation),severity(fatal),	/* { dg-error "expected end of line before ',' token" } */
						/* { dg-error "'pragma omp error' encountered: foo" "" { target *-*-* } .-1 } */
#pragma omp error message("foo"),at(compilation),severity(fatal),asdf	/* { dg-error "expected 'at', 'severity' or 'message' clause" } */
#pragma omp error at(compilation) at(compilation)	/* { dg-error "too many 'at' clauses" } */
#pragma omp error severity(fatal) severity(warning)	/* { dg-error "too many 'severity' clauses" } */
#pragma omp error message("foo") message("foo")		/* { dg-error "too many 'message' clauses" } */
#pragma omp error at(execution)			/* { dg-error "'#pragma omp error' with 'at\\\(execution\\\)' clause may only be used in compound statements" } */

struct S
{
  #pragma omp error at(execution) message("foo")/* { dg-error "'#pragma omp error' with 'at\\\(execution\\\)' clause may only be used in compound statements" } */
  int s;
};

int
foo (int i, int x, const char *msg)
{
  #pragma omp error message(msg)		/* { dg-error "'pragma omp error' encountered: <message unknown at compile time>" } */
  if (x)
    #pragma omp error at(execution)		/* { dg-error "'#pragma omp error' with 'at\\\(execution\\\)' clause may only be used in compound statements" } */
  i++;
  if (x)
    ;
  else
    #pragma omp error at(execution) severity(warning)	/* { dg-error "'#pragma omp error' with 'at\\\(execution\\\)' clause may only be used in compound statements" } */
  i++;
  switch (0)
    #pragma omp error severity(fatal) at(execution)	/* { dg-error "'#pragma omp error' with 'at\\\(execution\\\)' clause may only be used in compound statements" } */
    ;
  while (0)
    #pragma omp error at(execution)message("42 - 1")	/* { dg-error "'#pragma omp error' with 'at\\\(execution\\\)' clause may only be used in compound statements" } */
    i++;
  lab:
  #pragma omp error severity(warning) message("bar") at(execution)	/* { dg-error "'#pragma omp error' with 'at\\\(execution\\\)' clause may only be used in compound statements" } */
    i++;
  return i;
}
