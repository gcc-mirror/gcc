/* { dg-do compile } */

#include "dfp-dbg.h"

/* Check that the compiler generates the correct decimal float constants.  */

_Decimal32 a = 100.223df;
_Decimal32 b = -2.3df;
_Decimal64 c = 3.4e-4dd;
_Decimal64 d = -4.500dd;
_Decimal128 e = 5678901234567.89e+200dl;
_Decimal128 f = -678901.234e-6dl;

/* The first value is DPD, the second is BID.  The order differs depending
   on whether the target is big-endian or little-endian.  */

/* { dg-final { scan-assembler "(.long|.word)\t(572653859|822183807)\n" } } */

/* { dg-final { scan-assembler "(.long|.word)\t(-1572863965|-1308622825)\n" } } */

/* { dg-final { scan-assembler "(.long|.word)\t(52|34)\n" } } */
/* { dg-final { scan-assembler "(.long|.word)\t(572784640|824180736)\n" } } */

/* { dg-final { scan-assembler "(.long|.word)\t(4736|4500)\n" } } */
/* { dg-final { scan-assembler "(.long|.word)\t(-1574174720|-1319108608)\n" } } */

/* { dg-final { scan-assembler "(.long|.word)\t(-1975952433|957645077)\n" } } */
/* { dg-final { scan-assembler "(.long|.word)\t(190215|132222)\n" } } */
/* { dg-final { scan-assembler "(.long|.word)\t(574193664|835452928)\n" } } */

/* { dg-final { scan-assembler "(.long|.word)\t(931280180|678901234)\n" } } */
/* { dg-final { scan-assembler "(.long|.word)\t(-1576681472|-1339162624)\n" } } */
