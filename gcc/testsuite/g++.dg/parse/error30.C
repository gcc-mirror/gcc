// PR c++/30854
// { dg-do compile }
// { dg-options "-fshow-column -ansi -pedantic-errors -Wno-long-long" }

struct A
{
  A();
  A(int);
};

A a = -A();	// { dg-error "10:no match for.*operator-.*in.*-A\\(\\)" }
A b = -A(5);	// { dg-error "11:no match for.*operator-.*in.*-A\\(5\\)" }
