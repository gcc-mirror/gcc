// PR c++/17805

// Per 13.3.1.2/3 bullet 2, an operator function is not a candidate
// for overload resolution if neither argument is of class type and
// neither enumerator-typed argument gets an exact match, with or
// without reference binding, for the corresponding parameter.

struct A
{
  A(int);
  A(const char*);
};

bool operator==(const A&, const A&);
const A& operator*(const A&);

enum E { e };

bool b1 = (e == "");     // { dg-error "no match" }

bool b2 = (A(1) == "");

bool b3 = (e == A(1));

const A& a1 = *e;        // { dg-error "no match" }

const A& a2 = *A(1);
