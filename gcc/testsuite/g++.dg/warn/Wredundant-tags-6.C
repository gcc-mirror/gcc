/* PR c++/93824 - bogus -Wredundant-tags on a first declaration in use
   { dg-do compile }
   { dg-options "-Wredundant-tags" } */

extern class C1 &c1;              // { dg-bogus "\\\[-Wredundant-tags" }
extern class C1 &c1;              // { dg-warning "\\\[-Wredundant-tags" }

void fc2 (class C2);              // { dg-bogus "\\\[-Wredundant-tags" }
void fc2 (class C2);              // { dg-warning "\\\[-Wredundant-tags" }

const int
npc3 = sizeof (class C3*);        // { dg-bogus "\\\[-Wredundant-tags" }
const int
nppc3 = sizeof (class C3**);      // { dg-warning "\\\[-Wredundant-tags" }

extern struct S1 *s1p;            // { dg-bogus "\\\[-Wredundant-tags" }
extern struct S1 s1a[];           // { dg-warning "\\\[-Wredundant-tags" }

struct S3
{
  struct S3 *p1s3;                // { dg-warning "\\\[-Wredundant-tags" }
  S3 *p2s3;

  union U1 *p1u1;                 // { dg-bogus "\\\[-Wredundant-tags" }
  union U1 *p2u1;                 // { dg-warning "\\\[-Wredundant-tags" }
} s3;

typedef struct S3 T_S3;           // { dg-warning "\\\[-Wredundant-tags" }

typedef struct S4 T_S4;

struct S5
{
  struct S6
  {
  private:
    // 'struct' is redundant in a declaration of a pointer to ::S5;
    struct S5 *ps5;               // { dg-warning "\\\[-Wredundant-tags" }
    // 'struct' is required in a definition of a new type.
    struct S5 { } *ps5_2;
    struct S5 *p2s5_2;            // { dg-warning "\\\[-Wredundant-tags" }
  };
};


template <int> struct TS1;

// Verify redeclaration with no definition.
template <> struct TS1<0>;
template <> struct TS1<0>;

// Verify definition after a declaration and vice versa.
template <> struct TS1<1>;
template <> struct TS1<1> { };
template <> struct TS1<1>;

// Verify typedefs of an explicit specialization with a definition.
typedef struct TS1<1> TS1_1;      // { dg-warning "\\\[-Wredundant-tags" }
typedef        TS1<1> TS1_1;
typedef struct TS1<1> TS1_1;      // { dg-warning "\\\[-Wredundant-tags" }

// Verify object declarations of an expplicit specialization.
extern struct TS1<1> ts1_1;      // { dg-warning "\\\[-Wredundant-tags" }
extern        TS1<1> ts1_1;
extern struct TS1<1> ts1_1;      // { dg-warning "\\\[-Wredundant-tags" }

// Verify typedefs of an implicit specialization without a definition.
typedef struct TS1<2> TS1_2;      // { dg-warning "\\\[-Wredundant-tags" }
typedef        TS1<2> TS1_2;
typedef struct TS1<2> TS1_2;      // { dg-warning "\\\[-Wredundant-tags" }

// Verify object declarations of an implicit specialization.
extern struct TS1<2> ts1_2;       // { dg-warning "\\\[-Wredundant-tags" }
extern        TS1<2> ts1_2;
extern struct TS1<2> ts1_2;       // { dg-warning "\\\[-Wredundant-tags" }


// Verify partial template specialization.
template <class> struct TS2;
template <class T> struct TS2<const T>;
template <class T> struct TS2<volatile T>;

template <class T>
struct TS4
{
  typedef struct TS2<const T> TS2_CT1;   // { dg-warning "\\\[-Wredundant-tags" }
  typedef        TS2<const T> TS2_CT2;

  typedef struct TS2<T> TS2_T1;   // { dg-warning "\\\[-Wredundant-tags" }
  typedef        TS2<T> TS2_T2;
};
