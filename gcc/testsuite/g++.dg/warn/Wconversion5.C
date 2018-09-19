// PR c++/87357
// { dg-do compile }
// { dg-options "-Wconversion" }

struct B { };

struct X : public B {
  operator X(); // { dg-warning "3:conversion to the same type will never use a type conversion operator" }
  operator X&(); // { dg-warning "3:conversion to a reference to the same type will never use a type conversion operator" }
  operator X() const; // { dg-warning "3:conversion to the same type will never use a type conversion operator" }
  operator const X(); // { dg-warning "3:conversion to the same type will never use a type conversion operator" }

  operator B(); // { dg-warning "3:conversion to a base class will never use a type conversion operator" }
  operator B&(); // { dg-warning "3:conversion to a reference to a base class will never use a type conversion operator" }
  operator B() const; // { dg-warning "3:conversion to a base class will never use a type conversion operator" }
  operator const B(); // { dg-warning "3:conversion to a base class will never use a type conversion operator" }

  operator void(); // { dg-warning "3:conversion to void will never use a type conversion operator" }
  operator void() const; // { dg-warning "3:conversion to void will never use a type conversion operator" }
};
