// ensure that assert contract predicates that are not convertible to bool
// generate an error
// ensure the same for instatiated template functions
// ensure the same for non-instatiated template functions when the predicate
// is not dependent on the template parameters
// ensure template parameter dependent, potentially non-boolean, contract
// predicates do not generate an error if never instatiated
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

void fun()
{
  return;
}

template<typename T>
void fun2(T a)
{
  [[assert: fun()]]; // { dg-error "could not convert|in argument" }
}

template<typename T>
void fun3(T a)
{
  [[assert: fun()]]; // { dg-error "could not convert|in argument" }
}

template<typename T>
void fun4(T a)
{
  [[assert: a.fun()]];
}

struct test
{
  void fun() { }
  void fun2() { }
};

template<typename T>
void fun5(T a)
{
  [[assert: a.fun2()]]; // { dg-error "could not convert" }
}

struct VoidFun
{
  void fun() { }
};
struct BoolFun
{
  bool fun() { return true; }
};

template<typename T>
void fun6(T a)
{
  [[ assert: a.fun() ]]; // { dg-error "could not convert" }
}

template void fun6(VoidFun);

template<typename T>
void fun7(T a)
{
  [[ assert: a.fun() ]];
}

template void fun7(BoolFun);

struct ImplicitBool
{
  operator bool() { return true; }
};
struct ExplicitBool
{
  explicit operator bool() { return true; }
};

template<typename T>
void fun8(T a)
{
  [[ assert: T() ]];
}

template void fun8(ImplicitBool);
template void fun8(ExplicitBool);

void fun9()
{
  [[ assert: ImplicitBool() ]];
  [[ assert: ExplicitBool() ]];
}

int main()
{
  [[assert: fun()]]; // { dg-error "could not convert" }
  fun2(1);

  test t;
  fun5(t);
  return 0;
}
