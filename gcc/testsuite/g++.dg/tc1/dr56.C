// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR56: Redeclaring typedefs within classes 

class X { 
  typedef int I; 
  typedef int I;  // { dg-error "" "Cannot redeclare a typedef in a class scope" { xfail *-*-* } }
};

// In non-class scope, they are allowed.
typedef int A;
typedef int A;
