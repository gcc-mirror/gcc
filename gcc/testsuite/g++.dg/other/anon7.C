// PR c++/65879

static struct
{
  void f();
  struct Inner
  {
    void g();
  };
} x;
