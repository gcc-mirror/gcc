// { dg-do assemble  }
// PRMS Id: 6303
// Bug: compiler crashes processing the cleanup for arrayOfClass.

class Class {
public:
  ~Class();		// This dtor MUST be declared to generate the error...
};

Class varOfClass;

int main() {
  // This MUST be 'const' to generate the error...
  const Class	arrayOfClass[1] = { varOfClass }; // causes abort
}
