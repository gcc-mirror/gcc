/* { dg-do compile } */
/* { dg-options "-std=c89 -Wvla" } */

extern void 
func (int i, int array[i]); /* { dg-warning "variable length array 'array' is used" } */
