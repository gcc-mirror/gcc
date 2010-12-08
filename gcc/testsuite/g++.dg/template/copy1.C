// { dg-do compile }

// Origin: hkluender@otg.com

// PR 5189

struct A
{
  A(A&); // { dg-message "note" }
  template <class T> A(T); 	// { dg-message "note" }
};

A a = 0; // { dg-error "no matching function" }
// { dg-message "candidate" "candidate note" { target *-*-* } 13 }

