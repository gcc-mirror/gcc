/* { dg-do compile } */
/* { dg-options "-pedantic-errors -std=c89 -Wno-vla" } */

extern void 
func (int i, int array[i]);
