// Testcase for ambiguity between cast and parmlist.
// This ambiguity accounts for 1 of the r/r conflicts.
// Do not compile with -pedantic so that the compiler will accept taking
// the sizeof a function type.
// Special g++ Options: -Wno-pointer-arith
// Build don't link:

void f(){
  (void)sizeof(int((int)1.2));
  (void)sizeof(int((int)));		// gets bogus error - 
}
