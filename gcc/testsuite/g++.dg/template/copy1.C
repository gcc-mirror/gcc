// { dg-do compile }

// Origin: hkluender@otg.com

// PR 5189

struct A
{
  A(A&); // { dg-message "candidate" }
  template <class T> A(T); 
};

A a = 0; // { dg-error "no matching function" }

