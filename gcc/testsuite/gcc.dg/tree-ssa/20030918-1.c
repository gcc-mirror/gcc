/* The compiler was failing to adjust pointer dereferences into array
   references after propagating &equot[0] into p.  */

/* { dg-do compile } */
/* { dg-options "-O -ftree-dominator-opts" } */

static unsigned short equot[(6 +3)];
int
foo (num)
     unsigned short num[];
{
  unsigned short *p = &equot[0];
  *p++ = num[0];
  *p++ = num[1];
}
