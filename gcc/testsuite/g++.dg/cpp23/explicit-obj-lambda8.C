// P0847R7
// { dg-do run { target c++23 } }

// lambda capture mutability with explicit object parameter

void capture_by_value()
{
  static constexpr int magic = 42;
  auto f0 = [n = 0](this auto self){
    n += magic;
    return n;
  };
  auto f1 = [n = 0](this auto& self){
    n += magic;
    return n;
  };
  auto f2 = [n = 0](this auto&& self){
    n += magic;
    return n;
  };

  // passed by value, should still return a value equal to magic regardless
  // of how many times it is called
  if (f0 () != magic)
    __builtin_abort ();
  if (f0 () != magic)
    __builtin_abort ();
  // passed by reference, the returned value should increase by magic
  // each time it is called
  if (f1 () != magic)
    __builtin_abort ();
  if (f1 () != magic + magic)
    __builtin_abort ();
  if (f2 () != magic)
    __builtin_abort ();
  if (f2 () != magic + magic)
    __builtin_abort ();
}

void capture_by_ref()
{
  static constexpr int magic = 42;
  int n0 = 0;
  auto f0 = [&n0](this auto self){
    n0 += magic;
  };
  int n1 = 0;
  auto f1 = [&n1](this auto& self){
    n1 += magic;
  };
  int n2 = 0;
  auto f2 = [&n2](this auto&& self){
    n2 += magic;
  };
  int n3 = 0;
  auto f3 = [&n3](this auto const& self){
    n3 += magic;
  };

  // all calls should mutate their capture, the capture is by reference
  if (f0 (); n0 != magic)
    __builtin_abort ();
  if (f0 (); n0 != magic + magic)
    __builtin_abort ();

  if (f1 (); n1 != magic)
    __builtin_abort ();
  if (f1 (); n1 != magic + magic)
    __builtin_abort ();

  if (f2 (); n2 != magic)
    __builtin_abort ();
  if (f2 (); n2 != magic + magic)
    __builtin_abort ();

  if (f3 (); n3 != magic)
    __builtin_abort ();
  if (f3 (); n3 != magic + magic)
    __builtin_abort ();
}

int main()
{
  capture_by_value ();
  capture_by_ref ();
}

