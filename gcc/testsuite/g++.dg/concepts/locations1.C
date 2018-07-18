// { dg-additional-options "-std=c++17 -fconcepts" }

struct S
{
  concept S();  // { dg-error "3:a constructor cannot be .concept." }
  concept int s = 1;  // { dg-error "3:non-static data member .s. declared .concept." }
  concept void foo();  // { dg-error "3:a concept cannot be a member function" }
  concept ~S();  // { dg-error "3:a destructor cannot be .concept." }
};

typedef concept int my_int;  // { dg-error "9:.concept. cannot appear in a typedef declaration" }

void bar(concept int);  // { dg-error "10:a parameter cannot be declared .concept." }

concept int i = 0;  // { dg-error "1:a non-template variable cannot be .concept." }
