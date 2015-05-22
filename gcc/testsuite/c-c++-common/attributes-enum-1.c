/* Test enumerators with attributes.  */
/* PR c/47043 */
/* { dg-do compile } */

enum E {
  A __attribute__((deprecated)),
  B __attribute__((deprecated ("foo"))),
  C __attribute__((deprecated)) = 10,
  D __attribute__((deprecated ("foo"))) = 15,
  E
};

int
f (int i)
{
  i += A; /* { dg-warning ".A. is deprecated" } */
  i += B; /* { dg-warning ".B. is deprecated" } */
  i += C; /* { dg-warning ".C. is deprecated" } */
  i += D; /* { dg-warning ".D. is deprecated" } */
  i += E;
  return i;
}
