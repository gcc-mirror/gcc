// PR c++/94325
// { dg-do run { target c++11 } }
// { dg-options "-fsanitize=vptr -fno-sanitize-recover=vptr" }

struct A { virtual ~A () = default; };
struct B : public virtual A {};
struct C : public B {};
struct D : public C {};

int
main ()
{
  D a;
}
