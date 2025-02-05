/* { dg-additional-options "-mtune=pentium4" { target ia32 } } */

#include "tree-vect.h"

typedef unsigned char FcChar8;
typedef unsigned short FcChar16;
typedef unsigned int FcChar32;
typedef int FcBool;

#define FcFalse 0
#define FcTrue 1
#define FcDontCare 2

__attribute__((noipa))
static FcBool
FcLooksLikeSJIS (FcChar8 *string, int len)
{
    int     nhigh = 0, nlow = 0;

    while (len-- > 0)
    {
        if (*string++ & 0x80) nhigh++;
        else nlow++;
    }
    /*
     * Heuristic -- if more than 1/3 of the bytes have the high-bit set,
     * this is likely to be SJIS and not ROMAN
     */
    if (nhigh * 2 > nlow)
        return FcTrue;
    return FcFalse;
}

int main()
{
  check_vect ();
  unsigned char* s = "DejaVuMathTeXGyre-Regulardtd!";
  if (FcLooksLikeSJIS(s, 29))
    abort ();
  return 0;
}
