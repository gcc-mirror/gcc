struct X {
  int* rc;
  this (int n) { auto x = new int[](1); rc = x.ptr; *rc = n; }
  this (this)  { ++*rc; }
  ~this ()     { --*rc; }
  @disable void opAssign (X src);
}

struct Y {
  X x;
}

void frob(X x)
{
    Y y = { x: x };
    // The 'rc' counter starts from 1 and gets bumped when:
    // - 'f0' is passed to 'frob'
    // - 'y' is initialized with 'x'
    assert(*y.x.rc == 3);
}

void main ()
{
    auto f0 = X(1);
    frob(f0);
}
