struct Environment {
  struct AsyncHooks { // { dg-lto-warning "10: type 'struct AsyncHooks' violates the C\\+\\+ One Definition Rule" }
    int providers_[1]; // { dg-lto-message "the first difference of corresponding definitions is field 'providers_'" }
  };
  AsyncHooks async_hooks_;
};
void fn1() { Environment a; }
int main ()
{
}
