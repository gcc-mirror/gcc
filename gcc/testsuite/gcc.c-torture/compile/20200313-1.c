/* PR middle-end/92071 */
/* Testcase by David Binderman <dcb314@hotmail.com> */

void *a;
union U { double c; char d[8]; };
void bar (union U);

void
foo (void)
{
  union U b;
  __builtin_memcpy (b.d, a, 8);
  bar (b);
}
