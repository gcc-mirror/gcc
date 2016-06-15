// PR c++/70202

struct D { };

union U : virtual D { };  // { dg-error "derived union" }

struct A
{
  virtual ~A() {}
};

struct B : A, virtual U { };  // { dg-error "base type 'U' fails" }

struct C : A, B {};  // { dg-message "direct base 'A' inaccessible" }

C c;
