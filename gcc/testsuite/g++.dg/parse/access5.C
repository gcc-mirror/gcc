// { dg-do compile }

// Origin: Giovanni Bajo <giovannibajo@libero.it>

// PR c++/11174: Access checking on pointer to member data.

struct A 
{
protected:
  int a;			// { dg-error "protected" }
};

struct B : A 
{
  void foo() {
    (void)&A::a;		// { dg-error "this context" }
  }
};
