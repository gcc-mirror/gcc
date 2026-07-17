/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=power10 -O2 -mno-dense-math" } */

#include "mma-builtin-1.h"

/* { dg-final { scan-assembler-times {\mlxv\M} 40 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M} 12 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 40 } } */
/* { dg-final { scan-assembler-times {\mxxmfacc\M} 20 } } */
/* { dg-final { scan-assembler-times {\mxxmtacc\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxvbf16ger2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvbf16ger2nn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvbf16ger2np\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvbf16ger2pn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvbf16ger2pp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf16ger2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf16ger2nn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf16ger2np\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf16ger2pn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf16ger2pp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf32ger\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf32gernn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf32gernp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf32gerpn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf32gerpp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvi16ger2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvi16ger2pp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvi16ger2s\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvi16ger2spp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvi4ger8\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvi4ger8pp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvi8ger4\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvi8ger4pp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvi8ger4spp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvbf16ger2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvbf16ger2nn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvbf16ger2np\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvbf16ger2pn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvbf16ger2pp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf16ger2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf16ger2nn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf16ger2np\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf16ger2pn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf16ger2pp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf32ger\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf32gernn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf32gernp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf32gerpn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf32gerpp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvi16ger2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvi16ger2pp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvi16ger2s\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvi16ger2spp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvi4ger8\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvi4ger8pp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvi8ger4\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvi8ger4pp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvi8ger4spp\M} 1 } } */
