// { dg-do compile { target c++20 } }

struct S
{
  concept S();  // { dg-error "3:a constructor cannot be .concept." }
  // { dg-error "concept definition syntax" "" { target *-*-* } .-1 }
  concept int s = 1;  // { dg-error "3:non-static data member .s. declared .concept." }
  // { dg-error "concept definition syntax" "" { target *-*-* } .-1 }
  concept void foo();  // { dg-error "3:a concept cannot be a member function" }
  // { dg-error "concept definition syntax" "" { target *-*-* } .-1 }
  concept ~S();  // { dg-error "3:a destructor cannot be .concept." }
  // { dg-error "concept definition syntax" "" { target *-*-* } .-1 }
};

typedef concept int my_int;  // { dg-error "9:.concept. cannot appear in a typedef declaration" }
// { dg-error "concept definition syntax" "" { target *-*-* } .-1 }

void bar(concept int);  // { dg-error "10:a parameter cannot be declared .concept." }
// { dg-error "concept definition syntax" "" { target *-*-* } .-1 }

concept int i = 0;  // { dg-error "1:a non-template variable cannot be .concept." }
// { dg-error "concept definition syntax" "" { target *-*-* } .-1 }
