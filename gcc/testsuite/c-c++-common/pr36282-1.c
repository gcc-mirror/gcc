/* PR middle-end/36282 */
/* { dg-do compile } */

#pragma weak bar

extern void *baz (void *dest, const void *src, __SIZE_TYPE__ n);
extern __typeof (baz) baz __asm("bazfn"); /* { dg-bogus "asm declaration ignored due to conflict with previous rename" } */

void
foo (void)
{
}
