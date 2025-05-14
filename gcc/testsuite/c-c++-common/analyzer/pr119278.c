/* PR analyzer/119278 */
/* { dg-do compile } */

const unsigned char a[] = {
#define A 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16
  A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
};
const unsigned char b[] = {
#define B 16, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 1
  B, B, B, B, B, B, B, B, B, B, B, B, B, B, B, B
};
struct S { const unsigned char *s; };
void bar (struct S *);

void
foo (void)
{
  struct S t[] = { a, b };
  bar (t);
}
