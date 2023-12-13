// { dg-do compile }

// Origin: hkluender@otg.com

// PR 5189

struct A
{
  A(A&);			// { dg-message "A::A" "" { target c++14_down } }
  template <class T> A(T); 	// { dg-message "A::A" "" { target c++14_down } }
};

// { dg-error "reference" "" { target c++14_down } .+1 }
A a = 0; // { dg-error "no match" "" { target c++14_down } }

