// { dg-do assemble  }
// PRMS Id: 6303
// Bug: compiler crashes processing the cleanup for arrayOfClass.

class Klasse {
public:
  ~Klasse();		// This dtor MUST be declared to generate the error...
};

Klasse varOfClass;

int main() {
  // This MUST be 'const' to generate the error...
  const Klasse	arrayOfClass[1] = { varOfClass }; // causes abort
}
