// PR c++/30854
// { dg-do compile }
// { dg-options "-fshow-column -ansi -pedantic-errors -Wno-long-long" }

struct A
{
  A();
  A(int);
};

A a = -A();	// { dg-error "operand type is 'A'" }
A b = -A(5);	// { dg-error "operand type is 'A'" }
