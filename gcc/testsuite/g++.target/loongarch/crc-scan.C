/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64" } */

#include "crc.C"

/* { dg-final { scan-assembler-times "crc\\.w\\.b\\.w" 2 } } */
/* { dg-final { scan-assembler-times "crc\\.w\\.h\\.w" 2 } } */
/* { dg-final { scan-assembler-times "crc\\.w\\.w\\.w" 2 } } */
/* { dg-final { scan-assembler-times "crcc\\.w\\.b\\.w" 2 } } */
/* { dg-final { scan-assembler-times "crcc\\.w\\.h\\.w" 2 } } */
/* { dg-final { scan-assembler-times "crcc\\.w\\.w\\.w" 2 } } */
/* { dg-final { scan-assembler-not "crc\\.w\\.\[bhw\]\\.w\t\\\$r\[0-9\]+,\\\$r0" } } */
/* { dg-final { scan-assembler-not "crcc\\.w\\.\[bhw\]\\.w\t\\\$r\[0-9\]+,\\\$r0" } } */
