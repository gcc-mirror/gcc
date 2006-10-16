/* PR middle-end/20491 */

/* { dg-do compile } */
/* { dg-skip-if "" { hppa*64*-*-* } "*" "" } */

/* Combine used to introduce invalid subregs for the asm input, and
   we'd crash later on, when removing all subregs.  */

volatile unsigned short _const_32 [4] = {1,2,3,4};
void
evas_common_convert_yuv_420p_601_rgba()
{
  __asm__ __volatile__ ("" : : "X" (*_const_32));
}

