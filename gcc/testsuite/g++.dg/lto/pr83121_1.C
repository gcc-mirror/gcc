struct Environment {
  struct AsyncHooks {
    int providers_[1];
  };
  AsyncHooks async_hooks_;
};
void fn1() { Environment a; }
int main ()
{
}
