// PR c++/123331
// { dg-do compile }
// { dg-additional-options "-O2" }

struct A { virtual void foo () = 0; };
struct B { A a[1]; };	// { dg-error "cannot declare field 'B::a' to be of abstract type 'A'" }
// { dg-error "cannot construct an object of abstract type 'A'" "" { target *-*-* } .-1 }

template <typename T>
void
bar (T x)
{
} 

int
main ()
{
  B b;
  bar (b); 
}
