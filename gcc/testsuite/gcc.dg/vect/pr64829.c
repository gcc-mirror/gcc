/* { dg-do compile } */

typedef unsigned char Uint8;
typedef int Sint32;
typedef unsigned int Uint32;

typedef union RMColorDataRef
{
  Uint8* data8;
} RMColorDataRef;

typedef struct RMColorData
{
  Uint32 dataCount;
  RMColorDataRef dataRef;
} RMColorData;

typedef struct RMColorTable
{
  Uint8 dataCompsOut;
  RMColorDataRef dataRef;
} RMColorTable;

int fail ( const RMColorData * pInColor,
	   RMColorData * pOutColor,
	   const RMColorTable * pColorTable )
{
  Uint32 comp;
  Uint8 nCompOut;

  Sint32 result;

  Uint32 interpFrac1, interpFrac2, interpFrac3;
  Sint32 val0, val1, val2, val3;

  Uint8 * pOut;

  const Uint8 * pClutData;
  const Uint8 * pCornerPoint0;

  Uint8 lastOut[((8) > (4) ? (8) : (4))];

  pOut = pOutColor->dataRef.data8;
  pClutData = pColorTable->dataRef.data8;

  nCompOut = pColorTable->dataCompsOut;

  pCornerPoint0 = pClutData;

  for (comp = 0; comp < nCompOut; comp++)
    {
      val0 = *pCornerPoint0++;

      result = val0 << 4;

      result += (val1 - val0) * interpFrac1;
      result += (val2 - val1) * interpFrac2;
      result += (val3 - val2) * interpFrac3;

      *pOut++ = lastOut[comp] = (Uint8)(result >> 4);
    }

  return (0);
}

