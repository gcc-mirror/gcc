// { dg-lto-do link }
// { dg-lto-options {{-O0 -flto}} }
/* We need -O0 to avoid the "Environment" locals in the test functions
   from being optimized away.  */

struct Environment { // { dg-lto-warning "8: type 'struct Environment' violates the C\\+\\+ One Definition Rule" }
  struct AsyncHooks {
    int providers_[2]; // { dg-lto-message "a field of same name but different type is defined in another translation unit" }
  };
  AsyncHooks async_hooks_;
};
void fn2() { Environment a; }
