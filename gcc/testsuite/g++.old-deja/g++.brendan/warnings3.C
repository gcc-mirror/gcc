// { dg-do assemble  }
// { dg-options "-O2 -Wall" }
// GROUPS passed warnings
class A {
int i;
public:
        void funcA(void) { 
                funcB(); 
        }

	// The compiler should not emit a warning about not being
	// able to inline this function.
        void funcB(void) { 
                i++; 
        }
};
