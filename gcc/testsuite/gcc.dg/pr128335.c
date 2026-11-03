/* { dg-do compile } */
/* { dg-options "-std=c23" } */

// check that after a definition and inside a redefintion the type of
// the enumerator is the enumerated type (and not a compatible integer)
enum F { C = -1U };
static_assert(_Generic(C, enum F: true));
enum E { A = 1, B = _Generic(A, int: -1U) };
static_assert(_Generic(A, enum E: true, enum F: false));
static_assert(_Generic(B, enum E: true, enum F: false));
enum E { A = 1, B = _Generic(A, enum E: -1U, enum F: 1U) };
static_assert(_Generic(A, enum E: true, enum F: false));
static_assert(_Generic(B, enum E: true, enum F: false));

// check that after a definition and inside a redefintion the type of
// the enumerator is "int" (and not another compatible enumerated type)
enum I : int { II = 1 };
enum G { D = 1, E = _Generic(D, enum I: 2) };
static_assert(_Generic(D, enum I: true));
static_assert(_Generic(E, enum I: true));
enum G { D = 1, E = _Generic(D, enum I: 2) };
static_assert(_Generic(D, enum I: true));
static_assert(_Generic(E, enum I: true));

