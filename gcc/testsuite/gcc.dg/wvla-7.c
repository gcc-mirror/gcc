/* { dg-do compile } */
/* { dg-options "-pedantic-errors -std=c89 -Wvla" } */

extern void 
func (int i, int [i]); /* { dg-error "error: ISO C90 forbids variable" } */
