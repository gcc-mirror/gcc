/* { dg-do compile } */
/* { dg-require-effective-target fixed_point } */

extern _Fract sinuhk_deg (unsigned short _Accum);

_Fract cosuhk_deg (unsigned short _Accum deg)
{
  unsigned short _Accum _90_deg = 90uhk;
  __asm ("" : "+r" (_90_deg));

  return deg <= _90_deg
    ? sinuhk_deg (_90_deg - deg)
    : -sinuhk_deg (deg - _90_deg);
}
