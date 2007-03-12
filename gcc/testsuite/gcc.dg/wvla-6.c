/* { dg-do compile } */
/* { dg-options "-std=c89 -Wvla" } */

extern void 
func (int i, int [i]); /* { dg-warning "variable length array is used" } */
