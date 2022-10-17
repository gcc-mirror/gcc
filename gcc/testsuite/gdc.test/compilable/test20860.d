// https://issues.dlang.org/show_bug.cgi?id=20860

struct A
{
    this(int a) {}
  ///
  void opDispatch(string methodName, Params...)(Params params) {
  }

  ~this() {}
}

void main()
{
    A(3).test();
}
