namespace N
{
  struct A;
  int f() {
    struct N::A { // { dg-error "" }
      A() {}
    };
    return 0;
  }
}
