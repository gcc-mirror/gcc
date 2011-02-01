/* { dg-do compile } */
/* { dg-options "-O2" } */

#define FILL_BACK *ptrRGB++=0; *ptrRGB++=0; *ptrRGB++=0;

  
void uncompact(unsigned char* ptrRGB, const unsigned int* ptrSrc, const unsigned char* ptrRGBcompact, int line, int nbPixLeft)
{

#define BIT_2_RGB32                                                     \
  if ((v & 0x00000001)){ nbPixLeft--; *ptrRGB++ = *ptrRGBcompact++; *ptrRGB++ = *ptrRGBcompact++; *ptrRGB++ = *ptrRGBcompact++; } \
  else{ FILL_BACK }                                                     \
  v >>= 1;
#define BIT_2_RGB16                                                     \
  if ((v16 & 0x0001)){ nbPixLeft--; *ptrRGB++ = *ptrRGBcompact++; *ptrRGB++ = *ptrRGBcompact++; *ptrRGB++ = *ptrRGBcompact++; } \
  else{ FILL_BACK }                                                     \
  v16 >>= 1;				

  int x;
  unsigned int v, *ptrSrc32bits=(unsigned int*)ptrSrc;
  unsigned short v16,*ptrSrc16bits;

  for(x=0; x<line; x++) {
    v = *ptrSrc32bits++;
    BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32
    BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32
    BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32
    BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32 BIT_2_RGB32
  }

  ptrSrc16bits=(unsigned short *)ptrSrc32bits;
  v16 = *ptrSrc16bits++;
  BIT_2_RGB16	BIT_2_RGB16	BIT_2_RGB16	BIT_2_RGB16	BIT_2_RGB16	BIT_2_RGB16	
  BIT_2_RGB16	BIT_2_RGB16	BIT_2_RGB16	BIT_2_RGB16	BIT_2_RGB16	BIT_2_RGB16      
 
}
