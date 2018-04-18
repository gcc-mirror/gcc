struct Environment {
  struct AsyncHooks {
    int providers_[1];
  };
  AsyncHooks async_hooks_; // { dg-lto-message "a field of same name but different type is defined in another translation unit" }
};
void fn1() { Environment a; }
int main ()
{
}
