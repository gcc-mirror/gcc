// { dg-do assemble  }
// GROUPS passed nested-classes
class T {
public:
    typedef int I;
    class Y {int y;};
    typedef Y Z;
};

T::I i;
T::Y y;
T::Z z;
