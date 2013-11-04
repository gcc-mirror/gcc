// PR c++/29043
// { dg-do compile }

struct S		// { dg-error "uninitialized" "" { target c++11 } }
{
  int const i; // { dg-message "should be initialized" "" { target c++98 } }
};

class C
{
public:
  C() {} // { dg-error "uninitialized const member|deleted" }
  S s;
};

struct S2		// { dg-error "uninitialized" "" { target c++11 } }
{
  int& ref;   // { dg-message "should be initialized" "" { target c++98 } }
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

struct S4		// { dg-error "uninitialized" "" { target c++11 } }
{
  int const i; // { dg-message "should be initialized" "" { target c++98 } }
};

struct C4
{
  C4() {} // { dg-error "uninitialized const member|deleted" }
  S4 s4[ 1 ];
};

struct C5
{
  C5() {} // { dg-message "uninitialized" }   
  int const iit[ 1 ];
};

