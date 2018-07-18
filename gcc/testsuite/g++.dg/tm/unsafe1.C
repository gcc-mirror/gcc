// Transaction-unsafe testcase from TM TS.
// { dg-options -fgnu-tm }

struct S {
  virtual ~S();
};
void f() transaction_safe {
  S s;		     // { dg-error "unsafe" "invocation of unsafe destructor" }
}

int g(int x) { // is transaction-safe
  if (x <= 0)
    return 0;
  return x + g(x-1);
}
