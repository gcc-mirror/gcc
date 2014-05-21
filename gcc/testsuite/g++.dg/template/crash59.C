//PR c++/27329

template<int> struct A                          // { dg-message "forward declaration" }
!                                               // { dg-error "expected unqualified-id" }
  ;

template<int> struct A { int foo(); };          // { dg-error "not a template" }

int i = A<0>().foo();                           // { dg-error "not a template|invalid use" }


template<int> struct B        
!                                               // { dg-error "expected unqualified-id" }
  ;

template<int> struct B { static int bar(); };   // { dg-error "not a template" }

int j = B<0>::bar();                            // { dg-error "not a template|incomplete type" }
 
