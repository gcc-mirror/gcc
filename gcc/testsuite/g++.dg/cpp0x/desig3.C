// PR c++/82593
// { dg-do compile { target c++11 } }
// { dg-options "" }

const int INDEX1 = 0;
const int INDEX2 = 1;

class SomeClass {
public:
 SomeClass();
private:
 struct { int field; } member[2];
};

SomeClass::SomeClass()
 : member{
   [INDEX1] = { .field = 0 },
   [INDEX2] = { .field = 1 }
 }
{
}
