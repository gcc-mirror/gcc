/* { dg-options "-O1 -ffast-math -fdump-tree-recip" } */
/* { dg-do compile } */

struct MIOFILE {
  ~MIOFILE();
};
double potentially_runnable_resource_share();
void f1(double);
int make_scheduler_request(double a, double b)
{
  MIOFILE mf;
  double prrs = potentially_runnable_resource_share();
  f1(a/prrs);
  f1(1/prrs);
  f1(b/prrs);
}

/* { dg-final { scan-tree-dump-times " / " 1 "recip" } } */
