// { dg-do compile }
// Contributed by: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR 49: Non-constant pointers are invalid template arguments.

template<int *a> struct R { /* ... */ };
template<int b[5]> struct S { /* ... */ };

int p;
template struct R<&p>; // OK
template struct S<&p>; // OK due to parameter adjustment

int *ptr;
template struct R<ptr>; // { dg-error "constant" }
template struct S<ptr>; // { dg-error "constant" }

int v[5];
template struct R<v>; // OK due to implicit argument conversion
template struct S<v>; // OK due to both adjustment and conversion

