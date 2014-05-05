/* PR c/56989 */
/* { dg-do compile } */

extern void voidf (void);
extern int intf (void);

int
f (void)
{
  if (intf () < 0
      || voidf () < 0) /* { dg-error "10:void value not ignored as it ought to be" } */
    return 1;

  if (voidf () < 0 /* { dg-error "7:void value not ignored as it ought to be" } */
      || intf () < 0)
    return 1;

  return 0;
}
