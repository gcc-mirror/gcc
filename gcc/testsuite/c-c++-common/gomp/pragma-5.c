/* { dg-additional-options "-fdump-tree-original" }  */
/* PR preprocessor/103165  */

#define inner(...) #__VA_ARGS__ ; _Pragma   (	"   omp		error severity   (warning)	message (\"Test\") at(compilation)" ) /* { dg-line inner_location } */
#define outer(...) inner(__VA_ARGS__) /* { dg-line outer_location } */

void
f (void)
{
  const char *str = outer(inner(1,2)); /* { dg-line str_location } */
  /* { dg-warning "35:'pragma omp error' encountered: Test" "" { target *-*-* } inner_location }
     { dg-note "20:in expansion of macro 'inner'" "" { target *-*-* } outer_location }
     { dg-note "21:in expansion of macro 'outer'" "" { target *-*-* } str_location } */
}

#if 0
After preprocessing, the expected result are the following three lines:
     const char *str = "\"1,2\" ; _Pragma ( \"   omp		error severity   (warning)	message (\\\"Test\\\") at(compilation)\" )" ;
#pragma omp error severity(warning) message ("Test") at(compilation)
                                     ;
#endif

/* { dg-final { scan-tree-dump "const char \\* str = \\(const char \\*\\) \"\\\\\"1,2\\\\\" ; _Pragma \\( \\\\\"   omp\\\\t\\\\terror severity   \\(warning\\)\\\\tmessage \\(\\\\\\\\\\\\\"Test\\\\\\\\\\\\\"\\) at\\(compilation\\)\\\\\" \\)\";" "original" } }  */
