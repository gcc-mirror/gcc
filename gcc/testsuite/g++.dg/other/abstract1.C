// { dg-do compile }
// Contributed by <giovannibajo at gcc dot gnu dot org>,
//                <pavel_vozenilek at hotmail dot com>,
//                <bangerth at dealii dot org>
// c++/9256: Make sure that a pointer to an array of abstract elements
//  cannot be created, not even during template substitution (DR337).

// Changed massively by P0929R2: now only creating an object of the array type
// is ill-formed, not merely forming the array type.

struct Abstract { virtual void f() = 0; };  // { dg-message "note" } 
struct Complete { void f(); };


/*
 * TEST 1
 * Arrays of abstract elements cannot be defined.
 */

Abstract a0[2];        // { dg-error "" }
Abstract (*a1)[2];
Abstract (**a2)[2];
Abstract (***a3)[2];
Abstract *a4;
Abstract *a5[2];
Abstract (*a6[2])[2];
Abstract **a7[2];
Abstract *(*a8[2])[2];
Abstract (**a9[2])[2];

/*
 * TEST 2
 * If an array of abstract elements is created during template
 *  instantiation, an error should occur.
 */

template <class T> struct K {
  T (*a1)[2];
  T (a2)[2];   // { dg-error "abstract" }
};

template struct K<Abstract>;  // { dg-message "required" }



/*
 * TEST 3

 * Deducing an array of abstract elements during type deduction is no longer a
 *  silent failure.
 */

template <bool> struct StaticAssert;
template <> struct StaticAssert<true> {};

typedef char Yes;
typedef struct { char x[2]; } No;

template<typename U> No  is_abstract(U (*k)[1]);
template<typename U> Yes is_abstract(...);

StaticAssert<sizeof(is_abstract<Abstract>(0)) == sizeof(No)> b1;
StaticAssert<sizeof(is_abstract<Complete>(0)) == sizeof(No)> b2;
StaticAssert<sizeof(is_abstract<int>(0)) == sizeof(No)> b3;
