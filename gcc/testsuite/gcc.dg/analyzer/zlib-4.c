#include <stdlib.h>
#include <string.h>

typedef unsigned char Byte;
typedef unsigned int uInt;
typedef unsigned long uLong;

#define Z_NULL  0

int test ()
{
    uLong comprLen = 10000*sizeof(int);
    uLong uncomprLen = comprLen;
    Byte *compr    = (Byte*)calloc((uInt)comprLen, 1);
    Byte *uncompr  = (Byte*)calloc((uInt)uncomprLen, 1);
    if (compr == Z_NULL || uncompr == Z_NULL)
      {
	return 1; /* { dg-warning "leak of 'uncompr'" "uncompr leak" } */
	          /* { dg-warning "leak of 'compr'" "compr leak" { target *-*-* } .-1 } */
      }
    strcpy((char*)uncompr, "garbage");
    return 0; /* { dg-warning "leak of 'uncompr'" "uncompr leak" } */
	      /* { dg-warning "leak of 'compr'" "compr leak" { target *-*-* } .-1 } */
}
