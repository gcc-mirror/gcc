/* PR c/70852 */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

extern void *memset (void *, int, __SIZE_TYPE__);
extern int A[];
void
fn1 (void)
{
  memset (A, 0, 1);
}
