// { dg-do run  }
// g++ 1.36.1 bug 900208_02

// g++ does not allow a static member of a class/struct/union to be
// declared as an array without an explicit upper bound.

// Cfront 2.0 passes this test.

// keywords: static data members, arrays, dimension, array bound

class class0 {
public:
  static int class0_data_member_0[];	// { dg-bogus "" } 
};

int class0::class0_data_member_0[3] = { 1, 2, 3 };  // { dg-bogus "" } 

int main () { return 0; }
