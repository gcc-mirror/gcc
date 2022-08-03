/* { dg-additional-options "-fdump-tree-original" }  */
/* PR preprocessor/103165  */

#define inner(...) #__VA_ARGS__ ; _Pragma("omp error severity(warning) message (\"Test\") at(compilation)") /* { dg-line inner_location } */
#define outer(...) inner(__VA_ARGS__)

void
f (void)
{
  const char *str = outer(inner(1,2));
  /* { dg-warning "'pragma omp error' encountered: Test" "inner expansion" { target *-*-* } inner_location } */
}

#if 0
After preprocessing, the expected result are the following three lines:
     const char *str = "\"1,2\" ; _Pragma(\"omp error severity(warning) message (\\\"Test\\\") at(compilation)\")" ;
#pragma omp error severity(warning) message ("Test") at(compilation)
                                     ;
#endif

/* { dg-final { scan-tree-dump "const char \\* str = \\(const char \\*\\) \"\\\\\"1,2\\\\\" ; _Pragma\\(\\\\\"omp error severity\\(warning\\) message \\(\\\\\\\\\\\\\"Test\\\\\\\\\\\\\"\\) at\\(compilation\\)\\\\\"\\)\";" "original" } }  */
