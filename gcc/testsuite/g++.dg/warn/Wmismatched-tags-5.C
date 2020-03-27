/* PR c++/93810 - missing -Wmismatched-tags and -Wredundant-tags on a typedef
   of an implicit class template specialization
  { dg-do compile }
  { dg-options "-Wall -Wmismatched-tags" }
  { dg-require-effective-target c++11 } */

class A;                                // { dg-message "declared as 'class'" }
typedef        A A0;
typedef class  A A0;
typedef struct A A0;                    // { dg-warning "-Wmismatched-tags" }

template <int> struct B;                // { dg-message "declared as 'struct'" }
typedef        B<0> B0;
typedef class  B<0> B0;                 // { dg-warning "-Wmismatched-tags" }
typedef struct B<0> B0;


// Exercise member types of templates with non-type arguments.
template <int> struct CN;               // { dg-message "declared as 'struct'" }

template <int N>
struct X_CNp1 {
  typedef CN<N + 1> CNp1;
};

template <int N>
struct X_class_CNp1 {
  typedef class CN<N + 1> CNp1;         // { dg-warning "-Wmismatched-tags" }
};

template <int N>
struct X_struct_CNp1 {
  typedef struct CN<N + 1> CNp1;
};


// Exercise partial specialization of templates with member types.
template <class> class CT1;
template <class T> struct CT1<T*> { };
template <class T> struct CT1<T**> { };
template <class T> class  CT1<T***> { };

template <class> struct CT2;
template <class T> struct CT2<T*> {
  // Expect class-key to match the primary.
         CT1<T> ct1_0;
  class  CT1<T> ct1_1;
  struct CT1<T> ct1_2;                  // { dg-warning "-Wmismatched-tags" }

  // Expect class-key to match the CT1<T*> partial specialization.
         CT1<T*> ct1p1_0;
  class  CT1<T*> ct1p1_1;               // { dg-warning "-Wmismatched-tags" }
  struct CT1<T*> ct1p1_2;

  // Expect class-key to match the CT1<T**> partial specialization.
         CT1<T**> ct1p2_0;
  class  CT1<T**> ct1p2_1;              // { dg-warning "-Wmismatched-tags" }
  struct CT1<T**> ct1p2_2;

  // Expect class-key to match the CT1<T***> partial specialization.
         CT1<T***> ct1p3_0;
  class  CT1<T***> ct1p3_1;
  struct CT1<T***> ct1p3_2;             // { dg-warning "-Wmismatched-tags" }

  // Expect class-key to still match the CT1<T***> partial specialization.
         CT1<T****> ct1p4_0;
  class  CT1<T****> ct1p4_1;
  struct CT1<T****> ct1p4_2;            // { dg-warning "-Wmismatched-tags" }
};

// Exercise many partial specializations (since the class-key for each
// must be tracked separately from the others).
template <class>   class  D;
template <class T> struct D<T*>;
template <class T> class  D<T&>;
template <class T> struct D<const T*>;
template <class T> class  D<const T&>;
template <class T> struct D<volatile T*>;
template <class T> class  D<volatile T&>;
template <class T> struct D<const volatile T*>;
template <class T> class  D<const volatile T&>;

typedef class  D<int*> DIP;             // { dg-warning "-Wmismatched-tags" }
typedef struct D<int*> DIP;
typedef class  D<int*> DIP;             // { dg-warning "-Wmismatched-tags" }
typedef struct D<int*> DIP;

typedef class  D<int&>  DIR;
typedef struct D<int&> DIR;             // { dg-warning "-Wmismatched-tags" }
typedef class  D<int&>  DIR;


typedef struct D<const int*> DCIP;
typedef class  D<const int*> DCIP;      // { dg-warning "-Wmismatched-tags" }
typedef struct D<const int*> DCIP;

typedef struct D<const int&> DCIR;      // { dg-warning "-Wmismatched-tags" }
typedef class  D<const int&>  DCIR;
typedef struct D<const int&> DCIR;      // { dg-warning "-Wmismatched-tags" }


typedef struct D<volatile int*> DVIP;
typedef class  D<volatile int*> DVIP;   // { dg-warning "-Wmismatched-tags" }
typedef struct D<volatile int*> DVIP;

typedef struct D<volatile int&> DVIR;   // { dg-warning "-Wmismatched-tags" }
typedef class  D<volatile int&> DVIR;
typedef struct D<volatile int&> DVIR;   // { dg-warning "-Wmismatched-tags" }


typedef struct D<const volatile int*> DCVIP;
typedef class  D<const volatile int*> DCVIP;    // { dg-warning "-Wmismatched-tags" }
typedef struct D<const volatile int*> DCVIP;

typedef struct D<const volatile int&> DCVIR;    // { dg-warning "-Wmismatched-tags" }
typedef class  D<const volatile int&> DCVIR;
typedef struct D<const volatile int&> DCVIR;    // { dg-warning "-Wmismatched-tags" }
