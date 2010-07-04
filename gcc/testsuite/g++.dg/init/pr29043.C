// PR c++/29043
// { dg-do compile }

struct S 
{
  int const i; // { dg-message "should be initialized" }
};

class C
{
public:
  C() {} // { dg-error "uninitialized const member" }   
  S s;
};

struct S2
{
  int& ref; // { dg-message "should be initialized" }
};

class C2
{
public:
  C2() {} // { dg-error "uninitialized reference member" }   
  S2 s;
};

class C3
{
  C3() { }
  struct s {
    const int i;
  };
};

struct S4
{
  int const i; // { dg-message "should be initialized" }
};

struct C4
{
  C4() {} // { dg-error "uninitialized const member" }
  S4 s4[ 1 ];
};

struct C5
{
  C5() {} // { dg-message "uninitialized" }   
  int const iit[ 1 ];
};

