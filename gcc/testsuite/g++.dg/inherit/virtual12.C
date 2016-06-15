// PR c++/70202

union U { };

struct A
{
  virtual ~A() {}
};

struct B : A, virtual U { };  // { dg-error "base type 'U' fails" }

struct C : A, B {};  // { dg-message "direct base 'A' inaccessible" }

C c;
