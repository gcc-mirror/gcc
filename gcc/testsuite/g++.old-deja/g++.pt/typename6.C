// { dg-do assemble  }

template <class T>
struct A
{
  typedef T A_Type;
};


template <class U>
struct B : public A<U>
{
  A_Type Func();		// { dg-error "does not name a type" "err" }
  // { dg-message "note" "note" { target *-*-* } 13 }
};

template <class U>
A<U>::A_Type B<U>::Func()       // { dg-error "typename" } function
{				
}
