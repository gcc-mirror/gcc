// { dg-do assemble  }
// prms-id: 11482

void *vp;

enum E { bad, ok } e;

void foo() {
  e = (E)vp;		// { dg-error "" } 
}
