// { dg-do assemble  }

class X {
public:
  const operator int (); // { dg-error "" } invalid declaration.
};
