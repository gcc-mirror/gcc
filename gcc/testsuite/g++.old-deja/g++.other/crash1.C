class A
{
  enum B { ONE, TWO, THREE }; // ERROR - private
};

class A::B; // ERROR - A::B is not a class type, context
