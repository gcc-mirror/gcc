// Bug: g++ lies about DECL_CONTEXT, so the backend thinks B::f is not
// function-local.
// Contributed by Jason Merrill <jason@cygnus.com>
// excess errors test

struct A {
  virtual void f () = 0;
};

int main()
{
   struct B : public A {
     void f () { }
   }; 

   B b;
}
