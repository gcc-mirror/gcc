/* { dg-skip-if "not yet" { c++ } } */

void
fun1 (void)
{
#pragma acc parallel deviceptr(u) /* { dg-error "'u' undeclared" } */
  ;
#pragma acc kernels deviceptr(u[0:4]) /* { dg-error "expected '\\\)' before '\\\[' token" } */
  ;

#pragma acc data deviceptr(fun1) /* { dg-error "'fun1' is not a variable" } */
  ;
#pragma acc parallel deviceptr(fun1[2:5])
  /* { dg-error "'fun1' is not a variable" "not a variable" { target *-*-* } .-1 } */
  /* { dg-error "expected '\\\)' before '\\\[' token" "array" { target *-*-* } .-2 } */
  ;

  int i;
#pragma acc kernels deviceptr(i) /* { dg-error "'i' is not a pointer variable" } */
  ;
#pragma acc data deviceptr(i[0:4])
  /* { dg-error "'i' is not a pointer variable" "not a pointer variable" { target *-*-* } .-1 } */
  /* { dg-error "expected '\\\)' before '\\\[' token" "array" { target *-*-* } .-2 } */
  ;

  float fa[10];
#pragma acc parallel deviceptr(fa) /* { dg-error "'fa' is not a pointer variable" } */
  ;
#pragma acc kernels deviceptr(fa[1:5])
  /* { dg-error "'fa' is not a pointer variable" "not a pointer variable" { target *-*-* } .-1 } */
  /* { dg-error "expected '\\\)' before '\\\[' token" "array" { target *-*-* } .-2 } */
  ;

  float *fp;
#pragma acc data deviceptr(fp)
  ;
#pragma acc parallel deviceptr(fp[0:4]) /* { dg-error "expected '\\\)' before '\\\[' token" } */
  ;
}

void
fun2 (void)
{
  int i;
  float *fp;
#pragma acc kernels deviceptr(fp,u,fun2,i,fp)
  /* { dg-error "'u' undeclared" "u undeclared" { target *-*-* } .-1 } */
  /* { dg-error "'fun2' is not a variable" "fun2 not a variable" { target *-*-* } .-2 } */
  /* { dg-error "'i' is not a pointer variable" "i not a pointer variable" { target *-*-* } .-3 } */
  /* { dg-error "'fp' appears more than once in data clauses" "fp more than once" { target *-*-* } .-4 } */
  ;
}

void
fun3 (void)
{
  float *fp;
#pragma acc data deviceptr(fp,fp) /* { dg-error "'fp' appears more than once in data clauses" } */
  ;
#pragma acc parallel deviceptr(fp) deviceptr(fp) /* { dg-error "'fp' appears more than once in data clauses" } */
  ;
#pragma acc kernels copy(fp) deviceptr(fp) /* { dg-error "'fp' appears more than once in data clauses" } */
  ;
}

extern struct s s1;
extern struct s s2[1]; /* { dg-error "array type has incomplete element type" "" { target c } } */

void
fun4 (void)
{
  struct s *s1_p = &s1;
  struct s *s2_p = &s2;

#pragma acc parallel deviceptr(s1) /* { dg-error "'s1' is not a pointer variable" } */
  ;

#pragma acc parallel deviceptr(s2)
  ;

#pragma acc parallel deviceptr(s1_p)
  s1_p = 0;

#pragma acc parallel deviceptr(s2_p)
  s2_p = 0;
}

void
func5 (float *fp)
{
#pragma acc data deviceptr (fp)
  ;
}

void
func6 (float fp)
{
#pragma acc data deviceptr (fp)	/* { dg-error "is not a pointer variable" } */
  ;
}
