// { dg-do compile }
// PR c++/6936

struct Baser
{
    enum { j, i }; // { dg-error "inaccessible" }
};

struct Base : Baser
{
    static void j();
    static void i();
};

struct Derv : Base
{
  using Baser::j;
private:
  using Baser::i;
};

int k = Derv::j;
int l = Derv::i; // { dg-error "context" }
