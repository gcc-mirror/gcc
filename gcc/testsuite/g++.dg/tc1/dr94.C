// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR94: Inconsistencies in the descriptions of constant expressions 

struct S {
  static const int c = 5;
};
int a[S::c];

