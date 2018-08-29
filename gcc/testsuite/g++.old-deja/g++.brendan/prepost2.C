// { dg-do assemble  }
// GROUPS passed initialization
class foo {
public:
      operator ++ (); // { dg-error "" } no type or storage class
      operator ++ (int); // { dg-error "" } no type or storage class
      operator ++ (char);		// { dg-error "no type" }
// { dg-error "7:postfix .int foo::operator\\+\\+\\(char\\). must have .int. as its argument" "sec" { target *-*-* } .-1 }
      operator ++ (short);		// { dg-error "no type" }
// { dg-error "7:postfix .int foo::operator\\+\\+\\(short int\\). must have .int. as its argument" "sec" { target *-*-* } .-1 }
      operator ++ (long);		// { dg-error "no type" }
// { dg-error "7:postfix .int foo::operator\\+\\+\\(long int\\). must have .int. as its argument" "sec" { target *-*-* } .-1 }
};
