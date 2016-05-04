/* PR c/48778 */
/* { dg-do compile } */
/* { dg-options "-Waddress" } */

#define NULL ((void *) 0)

#define M1(b) ((b) != NULL ? 0 : (b))
#define M2(b) ((b) == NULL ? 0 : (b))
#define M3(b) (NULL != (b) ? 0 : (b))
#define M4(b) (NULL == (b) ? 0 : (b))

int
func (int b)
{
  if (M1 (&b) > 0)
    return 1;
  if (M2 (&b) > 0)
    return 2;
  if (M3 (&b) > 0)
    return 3;
  if (M4 (&b) > 0)
    return 4;
  return 0;
}
