// { dg-do assemble  }
// GROUPS passed initialization
class foo {
public:
      operator ++ (); // { dg-error "" } no type or storage class
      operator ++ (int); // { dg-error "" } no type or storage class
      operator ++ (char);		// illegal// { dg-error "" } .*
      operator ++ (short);		// illegal// { dg-error "" } .*
      operator ++ (long);		// illegal// { dg-error "" } .*
};
