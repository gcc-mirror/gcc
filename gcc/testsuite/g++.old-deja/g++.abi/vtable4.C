// Test for proper handling of extreme virtual inheritance.
// Previously we failed to recognise that in the constructor vtable
// for B_skel in C_skel, A_base was still primary to B_base, even though
// not to B_skel.

// From PR c++/3061.

struct A_base {
  virtual void foo() { }
};
class A_skel : virtual public A_base { };
 
class B_base : virtual public A_base { };
class B_skel : virtual public B_base, virtual public A_skel { };
 
class C_base : virtual public B_base { };
class C_skel : virtual public C_base, virtual public B_skel { };
 
class D_base : virtual public C_base { };
class D_skel : virtual public D_base, virtual public C_skel { };
 
class D_impl : virtual public D_skel { };
 
int main()
{
  D_impl i;
}
