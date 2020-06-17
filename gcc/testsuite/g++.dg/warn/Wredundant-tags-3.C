/* PR c++/93801 - False -Wmismatched-tags upon redundant typename
   { dg-do compile }
   { dg-options "-Wall -Wredundant-tags" } */

namespace N
{
  class C { };
  enum E { };
  struct S { };
  union U { };

  template <int> class TC { };
  template <int> struct TS { };
  template <int> union TU { };
}

N::C c1;
typename N::C c2;     // { dg-bogus "-Wredundant-tags" }
class N::C c3;        // { dg-warning "-Wredundant-tags" }

N::E e1;
typename N::E e2;     // { dg-bogus "-Wredundant-tags" }
enum N::E e3;         // { dg-warning "-Wredundant-tags" }

N::S s1;
typename N::S s2;     // { dg-bogus "-Wredundant-tags" }
struct N::S s3;       // { dg-warning "-Wredundant-tags" }

N::U u1;
typename N::U u2;     // { dg-bogus "-Wredundant-tags" }
                      // { dg-bogus "'class' tag used in naming 'union N::U" "pr93809" { xfail *-*-*} .-1 }
union N::U u3;        // { dg-warning "-Wredundant-tags" }


typedef N::TC<0> TC0;
typedef typename N::TC<0> TC0;
typedef class N::TC<0> TC0;   // { dg-warning "-Wredundant-tags" }

typedef N::TS<0> TS0;
typedef typename N::TS<0> TS0;
typedef struct N::TS<0> TS0;  // { dg-warning "-Wredundant-tags" }

typedef N::TS<0> TS0;
typedef typename N::TS<0> TS0;
typedef struct N::TS<0> TS0;  // { dg-warning "-Wredundant-tags" }
