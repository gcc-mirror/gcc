// { dg-do assemble  }
const int &f();
int &a = f();			// { dg-error "" } 
