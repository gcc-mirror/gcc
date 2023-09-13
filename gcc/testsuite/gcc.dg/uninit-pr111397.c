/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

int globalVar = 1;
int __attribute__ ((__returns_twice__)) test_setjmpex(void *context);

void testfn()
{
  int localVar = globalVar;
  while (!localVar) {
      test_setjmpex(__builtin_frame_address (0)); // { dg-bogus "uninitialized" }
      if (globalVar)
	break;
  }
}
