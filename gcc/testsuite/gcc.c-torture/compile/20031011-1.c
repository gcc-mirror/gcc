/* PR optimization/12544 */
/* Origin: Tony Hosking <hosking@cs.purdue.edu> */

/* Verify that non-local structures passed by invisible
   reference are correctly put in the stack.  */

typedef struct {
  int a;
  int f;
} A;

A *b;

void x (A a) {
  void y () {
    a.a = 0;
  }

  b = &a;
  y();
}
