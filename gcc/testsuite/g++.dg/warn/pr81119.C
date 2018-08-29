// PR c++/81119 Wshadow regression
// { dg-additional-options "-Wshadow" }

struct A;
typedef A A; // No warning, does not hide

struct B; // { dg-message "previous" }
typedef int B; // { dg-error "conflicting" }

struct C;
void C (); // { dg-warning "hides constructor" }
void C (int); // warning not repeated

struct D;
int D; // no warning, not a function

struct E;

enum X 
  {E}; // no warning, not a function
