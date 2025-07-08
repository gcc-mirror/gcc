// PR c++/93809
// { dg-do compile }

class C { };
enum E { };
struct S { };
union U { };

typedef typename ::C C2;
typedef typename ::E E2;
typedef typename ::S S2;
typedef typename ::U U2;
