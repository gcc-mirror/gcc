// { dg-do assemble  }
// Error: Internal compiler error on 1998/05/28 snapshot.

class foo {
  public:
  typedef int sometype;
};

struct die : public foo::sometype { // { dg-error "" } invalid base type
};
