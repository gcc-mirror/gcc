// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR80: Class members with same name as class 

struct A 
{
  int A;
};

struct A2
{
  static int A2;  // { dg-error "same name as" }
};


template <class>
struct A3
{
  int A3;
};

template <class>
struct A4
{
  static int A4;  // { dg-error "same name as" }
};


struct B
{
  B();
  int B;  // { dg-error "same name as" }
};

struct B2
{
  B2();
  static int B2;  // { dg-error "same name as" }
};

template <class>
struct B3
{
  B3();
  int B3;  // { dg-error "same name as" "this error should appear at parsing time" { xfail *-*-* } }
};

template <class>
struct B4
{
  B4();
  static int B4;  // { dg-error "same name as" }
};
