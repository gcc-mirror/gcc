// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR85: Redeclaration of member class

struct Base {
  struct Data {};  
  struct Data;  // { dg-error "" "redeclaration of nested class is invalid" { xfail *-*-* } }
};
