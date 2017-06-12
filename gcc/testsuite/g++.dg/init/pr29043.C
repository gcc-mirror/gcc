// PR c++/29043
// { dg-do compile }

struct S // { dg-message "implicitly deleted" "" { target c++11 } }
	 // { dg-error "uninitialized" "" { target c++11 } .-1 }
{
  int const i; // { dg-message "should be initialized" }
};

class C
{
public:
  C() {} // { dg-error "uninitialized const member|deleted" }
  S s;
};

struct S2 // { dg-message "implicitly deleted" "" { target c++11 } }
	  // { dg-error "uninitialized" "" { target c++11 } .-1 }
{
  int& ref;   // { dg-message "should be initialized" }
};

class C2
{
public:
  C2() {} // { dg-error "uninitialized reference member|deleted" }
  S2 s;
};

class C3
{
  C3() { }
  struct s {
    const int i;
  };
};

struct S4 // { dg-message "implicitly deleted" "" { target c++11 } }
	  // { dg-error "uninitialized" "" { target c++11 } .-1 }
{
  int const i; // { dg-message "should be initialized" }
};

struct C4
{
  C4() {} // { dg-error "uninitialized const member|deleted" }
  S4 s4[ 1 ];
};

struct C5
{
  C5() {} // { dg-error "uninitialized" }
  int const iit[ 1 ];
};

