// PR c++/30854
// { dg-do compile }

struct A
{
  A();
  A(int);
};

A a = -A();	// { dg-error "no match for.*operator-.*in.*-A\\(\\)" }
A b = -A(5);	// { dg-error "no match for.*operator-.*in.*-A\\(5\\)" }
