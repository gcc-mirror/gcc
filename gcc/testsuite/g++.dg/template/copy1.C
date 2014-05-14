// { dg-do compile }

// Origin: hkluender@otg.com

// PR 5189

struct A
{
  A(A&);			// { dg-message "A::A" }
  template <class T> A(T); 	// { dg-message "A::A" }
};

A a = 0; // { dg-error "" }

