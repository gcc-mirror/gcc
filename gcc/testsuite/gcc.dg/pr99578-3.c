/* PR middle-end/99578 */
/* { dg-do compile { target size32plus } } */
/* { dg-options "-O2 -Wstringop-overread" } */

struct S { unsigned int s; };
extern struct S v;
extern void *memcpy (void *, const void *, __SIZE_TYPE__);

void
foo (void)
{
  memcpy (&v, (void *)(0xe8ffc000), sizeof (struct S));	/* { dg-bogus "from a region of size 0" } */
}
