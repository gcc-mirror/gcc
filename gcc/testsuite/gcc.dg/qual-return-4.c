/* Test for warnings for qualified function return types.  -pedantic
   test.  Only the definition gets a warning for qualified void return
   types, not other such types within the definition.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-pedantic -std=gnu99" } */

volatile void (*y)(int);

volatile void (*vvf(int x))(int) { return y; }
