// PR c++/91264
// { dg-do compile { target c++14 } }

struct B {
  B() = default;
  int i;
};

constexpr B bar()
{
    constexpr B b = B(); // { dg-message "originally declared" }
    B *p = const_cast<B*>(&b);

    p->i = 11; // { dg-error "modifying a const object" }

   return *p;
}

void foo()
{  
   constexpr B y = bar(); // { dg-message "in .constexpr. expansion of" }
}
