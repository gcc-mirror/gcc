#include <stdlib.h>
#include <string.h>

typedef unsigned char Byte;
typedef unsigned int uInt;
typedef unsigned long uLong;

#define Z_NULL  0

void test ()
{
    uLong comprLen = 10000*sizeof(int);
    uLong uncomprLen = comprLen;
    Byte *compr    = (Byte*)calloc((uInt)comprLen, 1);
    Byte *uncompr  = (Byte*)calloc((uInt)uncomprLen, 1);
    if (compr == Z_NULL || uncompr == Z_NULL)
      exit (1);
    strcpy((char*)uncompr, "garbage");
    exit (0);
}
