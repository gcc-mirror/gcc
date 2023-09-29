// PR c++/57397
// { dg-do compile { target c++11 } }

template<class T1, class... Tn>
void foo(T1, Tn...);
// { dg-message "candidate expects at least 1 argument, 0 provided" "" { target *-*-* } .-1 }

template<class T1, class T2, class... Tn>
void bar(T1, T2, Tn...);
// { dg-message "candidate expects at least 2 arguments, 0 provided" "" { target *-*-* } .-1 }
// { dg-message "candidate expects at least 2 arguments, 1 provided" "" { target *-*-* } .-2 }

int main()
{
  foo();   // { dg-error "no matching" }
  foo(1);
  foo(1, 2);
  bar();   // { dg-error "no matching" }
  bar(1);  // { dg-error "no matching" }
  bar(1, 2);
  bar(1, 2, 3);
}
