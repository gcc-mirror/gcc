/* { dg-do compile { target c++11 } } */
/* { dg-additional-options "-fdump-tree-original" } */

/* Check mapper binding clauses.  */

struct Y {
  int z;
};

struct Z {
  int z;
};

[[omp::directive (declare mapper (struct Y y) map(tofrom: y))]];
[[omp::directive (declare mapper (struct Z z) map(tofrom: z))]];

int foo (void)
{
  struct Y yy;
  struct Z zz;
  int dummy;

  [[omp::directive (target data map(dummy))]]
  {
    [[omp::directive (target)]]
    {
      yy.z++;
      zz.z++;
    }
    yy.z++;
  }
  return yy.z;
}

struct P
{
  struct Z *zp;
};

int bar (void)
{
  struct Y yy;
  struct Z zz;
  struct P pp;
  struct Z t;
  int dummy;

  pp.zp = &t;

  [[omp::directive (declare mapper (struct Y y) map(tofrom: y.z))]];
  [[omp::directive (declare mapper (struct Z z) map(tofrom: z.z))]];

  [[omp::directive (target data map(dummy))]]
  {
    [[omp::directive (target)]]
    {
      yy.z++;
      zz.z++;
    }
    yy.z++;
  }

  [[omp::directive (declare mapper(struct P x) map(to:x.zp), map(tofrom:*x.zp))]];

  [[omp::directive (target)]]
  {
    zz = *pp.zp;
  }

  return zz.z;
}

/* { dg-final { scan-tree-dump-times {mapper_binding\(struct Y,omp declare mapper ~1Y\) mapper_binding\(struct Z,omp declare mapper ~1Z\)} 2 "original" { target c++ } } } */
/* { dg-final { scan-tree-dump {mapper_binding\(struct Z,omp declare mapper ~1Z\) mapper_binding\(struct P,omp declare mapper ~1P\)} "original" { target c++ } } } */
