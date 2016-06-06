// PR c++/70847
// { dg-do compile }

struct D { virtual D& f(); };

void
g()
{
  D d;
  d.f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f().f();
}
