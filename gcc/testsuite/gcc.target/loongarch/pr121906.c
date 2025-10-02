/* PR target/121906 */
/* { dg-do run } */
/* { dg-options "-O2 -mno-lsx" } */

typedef unsigned short u16;
typedef unsigned long u64;
typedef u16 v4hi __attribute__ ((vector_size (8)));
typedef u16 v8hi __attribute__ ((vector_size (16)));

u64 d;
int e, i;
u16 x;

int
main ()
{
  v4hi n = { 1 };
  u64 *o = &d;
p:
  asm goto ("" : : : : q);
  n[3] = (-(v8hi){ 0, 0, 0, 0, x })[7];
  for (; e >= 0; e--)
    {
      *o = n[0];
      if (i)
        goto p;
    q:
    }
  if (d != 1)
    __builtin_trap ();
}
