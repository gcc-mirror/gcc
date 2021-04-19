/* PR target/91710 */
/* { dg-do compile } */

struct S { unsigned int i:4; };

unsigned int test1(struct S s) {	/* { dg-bogus "parameter passing for argument of type" } */
  return s.i;
}

unsigned int test2(unsigned x, struct S s) {	/* { dg-bogus "parameter passing for argument of type" } */
  return x - s.i;
}

unsigned int test3(unsigned x, unsigned y, struct S s) {	/* { dg-bogus "parameter passing for argument of type" } */
  return x - y - s.i;
}
