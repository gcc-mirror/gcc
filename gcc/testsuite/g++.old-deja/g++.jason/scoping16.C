// { dg-do assemble  }
struct A {
  int a();			// { dg-error "" } 
  int a;			// { dg-error "" } 
};
