class A
{
        enum B { ONE, TWO, THREE };
};

class A::B; // ERROR - A::B is not a class type
