// { dg-options "-O2" }

void a (void (*f)())
{
  f();
}

struct RunState
{
  static void runcallback() { }
  static void wait()
  {
    a (runcallback);
  }
};

int main()
{
  RunState::wait();
  return 0;
}
