/* { dg-do compile } */
/* { dg-options "-freorder-blocks -ftracer" } */

extern int data[];
extern int i;

void
foo (void)
{
  char buf[8];
  __builtin___memcpy_chk (buf, data, i ? 8 : 4,
			  __builtin_object_size (buf, 0));
  __builtin___memcpy_chk (buf, data, i ? 8 : 4,
			  __builtin_object_size (buf, 0));
}
