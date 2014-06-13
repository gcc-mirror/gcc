/* { dg-do compile } */
/* { dg-options "-Os -g -Wno-pointer-to-int-cast" } */

/* This testcase exposes PR52472. expand_debug_expr mistakenly
   considers the address space of data to be generic, and
   asserts that PSImode pointers aren't valid in the generic 
   address space. */

extern const __memx unsigned data[][10];

unsigned long ice (void)
{
  unsigned long addr32;

  return addr32 = ((unsigned long) data);
}
