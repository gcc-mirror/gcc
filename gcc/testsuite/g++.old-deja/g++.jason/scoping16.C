// { dg-do assemble  }
struct A {
  int a();			// { dg-message "" }
  int a;			// { dg-error "" } 
};
