// PR c++/58810

typedef int F() const;

F f;           // { dg-error "cv-qualifier" }

struct A
{
  friend F f;  // { dg-error "cv-qualifier" }
};
