// PR c++/34094
// { dg-do link }
// { dg-options "-g" }

namespace {
  struct c
  {
    static const bool t = 0;
  };
}

const bool &f()
{
  return c::t;			// { dg-error "undefined" }
}

int main(void)
{
  return 0;
}

