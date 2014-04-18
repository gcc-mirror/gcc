// Core DR 1604/1571/1572
// { dg-require-effective-target c++11 }

struct Banana { };
struct Enigma { operator const Banana(); };
struct Doof { operator Banana&(); };
void enigmatic() {
  typedef const Banana ConstBanana;
  Banana &&banana1 = ConstBanana(); // { dg-error "" }
  Banana &&banana2 = Enigma();      // { dg-error "" }
  Banana &&banana3 = Doof();        // { dg-error "" }
}

class A {
public:
  operator volatile int &();
};
A a;

const int & ir1a = a.operator volatile int&(); // { dg-error "" }
const int & ir2a = a;			       // { dg-error "" }

struct X {
  operator int&();
} x;
int&& rri2 = X();		// { dg-error "" }
