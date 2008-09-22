/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

void SexiALI_Convert(void *vdest, void *vsrc, unsigned int frames)
{
 unsigned int x;
 short *src = vsrc;
 unsigned char *dest = vdest;
 for(x=0;x<256;x++)
 {
  int tmp;
  tmp = *src;
  src++;
  tmp += *src;
  src++;
  *dest++ = tmp;
  *dest++ = tmp;
 }
}
/* { dg-final { cleanup-tree-dump "vect" } } */

