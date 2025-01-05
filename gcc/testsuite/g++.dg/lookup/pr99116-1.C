// PR 99116 sliding hidden friends under template parm scopes

template<int T> struct Z {

  friend struct T; // { dg-bogus "shadows template parameter" }
};

struct Y {

  template<typename S> struct A {};

  friend struct S;
};

struct X
{
  struct S2 {};
  
  struct In
  {
    friend struct S2;
  };
};

typedef int S2;
