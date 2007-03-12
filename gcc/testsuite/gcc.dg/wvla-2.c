/* { dg-do compile } */
/* { dg-options "-std=c99 -Wvla" } */

extern void 
func (int i, int array[i]); /* { dg-warning "variable length array 'array' is used" } */
