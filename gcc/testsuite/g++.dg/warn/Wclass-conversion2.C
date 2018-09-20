// PR c++/87357
// { dg-do compile }
// { dg-options "-Wno-class-conversion" }

struct B { };

struct X : public B {
  operator X(); // { dg-bogus "3:converting .X. to the same type will never use a type conversion operator" }
  operator X&(); // { dg-bogus "3:converting .X. to a reference to the same type will never use a type conversion operator" }
  operator X() const; // { dg-bogus "3:converting .X. to the same type will never use a type conversion operator" }
  operator const X(); // { dg-bogus "3:converting .X. to the same type will never use a type conversion operator" }

  operator B(); // { dg-bogus "3:converting .X. to a base class .B. will never use a type conversion operator" }
  operator B&(); // { dg-bogus "3:converting .X. to a reference to a base class .B. will never use a type conversion operator" }
  operator B() const; // { dg-bogus "3:converting .X. to a base class .B. will never use a type conversion operator" }
  operator const B(); // { dg-bogus "3:converting .X. to a base class .const B. will never use a type conversion operator" }

  operator void(); // { dg-bogus "3:converting .X. to .void. will never use a type conversion operator" }
  operator void() const; // { dg-bogus "3:converting .X. to .void. will never use a type conversion operator" }
};
