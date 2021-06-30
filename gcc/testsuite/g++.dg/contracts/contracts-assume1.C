// test that assumed contracts do instatiate templates
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

template<typename T>
int f(T t)
{
  return -t;
}

int dummy()
{
  [[ assert assume: f(1.0) > 0 ]];
  return -1;
}

template<>
int f(double t) // { dg-error "specialization of.*after instantiation" }
{
  return -1.0;
}

int main()
{
  dummy();
  f(1);
  f(1.0);
  return 0;
}

