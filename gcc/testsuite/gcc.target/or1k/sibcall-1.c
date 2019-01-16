/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Just create some dummy call that should trigger sibcall, no
   stack logic.  */
int calc (int a, int b, int c) {
  if (c <= 0) return a;
  return calc (a * b, b, --c);
}

int main() {
   return calc (4, 3, 4);
}

/* Ensure sibcalls do not need to manipulate the stack.  */
/* { dg-final { scan-assembler-not "r1," } } */
/* Ensure sibcall maintains the body of the function.  */
/* { dg-final { scan-assembler "l.mul" } } */
