/* { dg-do compile } */
/* { dg-options "-std=c89 -Wvla" } */

extern void 
func (int i, int array[i]); /* { dg-warning "ISO C90 forbids variable length array 'array'" } */
