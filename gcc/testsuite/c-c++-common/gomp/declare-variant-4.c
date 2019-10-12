double f1 (int, long, float);
double f2 (int, long, float);
double f3 (int, long, float);
double f4 (int, long, float);
double f5 (int, long, float);

#pragma omp declare variant (f1) match (user={condition(1)})
#pragma omp declare variant (f2) match (user={condition(score(1):1)})
#pragma omp declare variant (f3) match (user={condition(score(3):1)})
#pragma omp declare variant (f4) match (user={condition(score(2):1)})
#pragma omp declare variant (f5) match (implementation={vendor(gnu)})
double
f6 (int x, long y, float z)
{
  return z + x + y;
}

double
test (int x)
{
  return f6 (x, x, 3.5f);
}
