/* PR rtl-optimization/33822 */
/* Origin: Andrew Pinski <pinskia@gcc.gnu.org> */

/* { dg-do compile } */
/* { dg-options "-O -g" } */
/* { dg-options "-O -g -mstrict-align" { target { powerpc*-*-linux* powerpc*-*-elf* } } } */

void ProjectOverlay(const float localTextureAxis[2], char *lump)
{
   const void *d = &localTextureAxis;
   int size = sizeof(float)*7 ;
   __builtin_memcpy( &lump[ 0 ], d, size );  /* { dg-warning "reading" } */
}
