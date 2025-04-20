/* { dg-do compile { target c++ } } */
/* { dg-additional-options "-fdump-tree-original" } */

/* Check mapper binding clauses.  */

struct Y {
  int z;
};

struct Z {
  int z;
};

#pragma omp declare mapper (struct Y y) map(tofrom: y)
#pragma omp declare mapper (struct Z z) map(tofrom: z)

int foo (void)
{
  struct Y yy;
  struct Z zz;
  int dummy;

#pragma omp target data map(dummy)
  {
  #pragma omp target
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

#pragma omp declare mapper (struct Y y) map(tofrom: y.z)
#pragma omp declare mapper (struct Z z) map(tofrom: z.z)

#pragma omp target data map(dummy)
  {
  #pragma omp target
    {
      yy.z++;
      zz.z++;
    }
    yy.z++;
  }

  #pragma omp declare mapper(struct P x) map(to:x.zp) map(tofrom:*x.zp)

  #pragma omp target
  {
    zz = *pp.zp;
  }

  return zz.z;
}

/* { dg-final { scan-tree-dump-times {mapper_binding\(struct Y,omp declare mapper ~1Y\) mapper_binding\(struct Z,omp declare mapper ~1Z\)} 2 "original" { target c++ } } } */
/* { dg-final { scan-tree-dump {mapper_binding\(struct Z,omp declare mapper ~1Z\) mapper_binding\(struct P,omp declare mapper ~1P\)} "original" { target c++ } } } */

/* { dg-final { scan-tree-dump {mapper_binding\(struct Z,#pragma omp declare mapper \(struct Z z\) map\(tofrom:z\)\) mapper_binding\(struct Y,#pragma omp declare mapper \(struct Y y\) map\(tofrom:y\)\)} "original" { target c } } } */
/* { dg-final { scan-tree-dump {mapper_binding\(struct Z,#pragma omp declare mapper \(struct Z z\) map\(tofrom:z\.z\)\) mapper_binding\(struct Y,#pragma omp declare mapper \(struct Y y\) map\(tofrom:y\.z\)\)} "original" { target c } } } */
/* { dg-final { scan-tree-dump {mapper_binding\(struct P,#pragma omp declare mapper \(struct P x\) map\(tofrom:\(x\.zp\)\[0:1\]\) map\(to:x.zp\)\) mapper_binding\(struct Z,#pragma omp declare mapper \(struct Z z\) map\(tofrom:z\.z\)\)} "original" { target c } } } */
