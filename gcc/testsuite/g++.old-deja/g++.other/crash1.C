// { dg-do assemble  }
class A
{
  enum B { ONE, TWO, THREE }; // { dg-error "" } private
};

class A::B; // { dg-error "" } A::B is not a class type, context
