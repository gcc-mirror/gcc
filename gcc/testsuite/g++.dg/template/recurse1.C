// PR c++/5050
//  Origin: georg.wild@gmx.de 
//  Reduced by: tbagot@bluearc.com and Nathanael C. Nerode <neroden@twcny.rr.com>
// Test for that excessive template recursion does not occur
// because of optimization.
// { dg-options "-ftemplate-depth-1 -O" }

 struct ostream  {
    template<class T> ostream& foo( const T & ) 
     { return *this;  }
  };
  
  void foo()  {
    ostream os;
    (os.foo(1)).foo(2);
  }
