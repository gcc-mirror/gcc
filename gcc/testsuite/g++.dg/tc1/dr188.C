// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR188: Comma operator and rvalue conversion

template <bool> struct StaticAssert;
template <> struct StaticAssert<true> {};

char arr[100];
StaticAssert<(sizeof(0,arr) == 100)> check;
