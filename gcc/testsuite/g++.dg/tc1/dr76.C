// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR76: Are const volatile variables considered "constant expressions"? 

volatile const int a = 5;

template <int> struct K;
template struct K<a>;	// { dg-error "" }
