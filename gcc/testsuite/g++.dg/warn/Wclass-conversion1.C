// PR c++/87357
// { dg-do compile }

struct B { };

struct X : public B {
  operator X(); // { dg-warning "3:converting .X. to the same type will never use a type conversion operator" }
  operator X&(); // { dg-warning "3:converting .X. to a reference to the same type will never use a type conversion operator" }
  operator X() const; // { dg-warning "3:converting .X. to the same type will never use a type conversion operator" }
  operator const X(); // { dg-warning "3:converting .X. to the same type will never use a type conversion operator" }

  operator B(); // { dg-warning "3:converting .X. to a base class .B. will never use a type conversion operator" }
  operator B&(); // { dg-warning "3:converting .X. to a reference to a base class .B. will never use a type conversion operator" }
  operator B() const; // { dg-warning "3:converting .X. to a base class .B. will never use a type conversion operator" }
  operator const B(); // { dg-warning "3:converting .X. to a base class .const B. will never use a type conversion operator" }

  operator void(); // { dg-warning "3:converting .X. to .void. will never use a type conversion operator" }
  operator void() const; // { dg-warning "3:converting .X. to .void. will never use a type conversion operator" }
};
