// PR c++/91264
// { dg-do compile { target c++14 } }

struct B {
  int i;
  double d;
};

constexpr B bar()
{
    constexpr B b{}; // { dg-message "originally declared" }
    B *p = const_cast<B*>(&b);

    p->i = 11; // { dg-error "modifying a const object" }
    p->d = 11.11;

   return *p;
}

void foo()
{  
   constexpr B y = bar(); // { dg-message "in .constexpr. expansion of" }
}
