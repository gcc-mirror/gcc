/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2 -O2" } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2bf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasph2hf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2bf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2bf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2bf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2bf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2bf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2bf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2bf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2bf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2bf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2bf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2bf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2bf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2hf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2hf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2hf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2hf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2hf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2hf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2hf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2hf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2hf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2hf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2hf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvt2ph2hf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ph\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ph\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ph\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ph\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ph\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ph\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2bf8x\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2bf8x\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2bf8x\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2bf8y\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2bf8y\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2bf8y\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2bf8sx\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2bf8sx\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2bf8sx\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2bf8sy\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2bf8sy\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2bf8sy\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2hf8x\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2hf8x\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2hf8x\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2hf8y\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2hf8y\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2hf8y\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2hf8sx\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2hf8sx\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2hf8sx\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2hf8sy\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2hf8sy\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2hf8sy\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovsxbw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%ymm\[0-9\](?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpsllw\[ \t]\+\\\$8, %ymm\[0-9]\+, %ymm\[0-9]\+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpsllw\[ \t]\+\\\$8, %ymm\[0-9]\+, %ymm\[0-9]\+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovsxbw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovsxbw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpsllw\[ \t]\+\\\$8, %xmm\[0-9]\+, %xmm\[0-9]\+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpsllw\[ \t]\+\\\$8, %xmm\[0-9]\+, %xmm\[0-9]\+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovsxbw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128 x1,a1,b1;
volatile __m256 x2,a2,b2;
volatile __m128h y,x128h,z;
volatile __m256h y2,x256h,z2;
volatile __m128i x128i,z3;
volatile __m256i x256i;
volatile __mmask8 m8;
volatile __mmask16 m16;
volatile __mmask32 m32;
const void *a;
__m128bh *b;
__m256bh *c;
__m128h *d;
__m256h *e;

void extern
avx10_2_test (void)
{
  y = _mm_cvtx2ps_ph (a1, b1);
  y = _mm_mask_cvtx2ps_ph (y, m8, a1, b1);
  y = _mm_maskz_cvtx2ps_ph (m8, a1, b1);

  y2 = _mm256_cvtx2ps_ph (a2, b2);
  y2 = _mm256_mask_cvtx2ps_ph (y2, m16, a2, b2);
  y2 = _mm256_maskz_cvtx2ps_ph (m16, a2, b2);

}

void extern
avx10_2_vcvtbiasph2bf8_test (void)
{
  x128i = _mm_cvtbiasph_bf8 (x128i, x128h);
  x128i = _mm_mask_cvtbiasph_bf8 (x128i, m8, x128i, x128h);
  x128i = _mm_maskz_cvtbiasph_bf8 (m8, x128i, x128h);

  x128i = _mm256_cvtbiasph_bf8 (x256i, x256h);
  x128i = _mm256_mask_cvtbiasph_bf8 (x128i, m16, x256i, x256h);
  x128i = _mm256_maskz_cvtbiasph_bf8 (m16, x256i, x256h);
}

void extern
avx10_2_vcvtbiasph2bf8s_test (void)
{
  x128i = _mm_cvts_biasph_bf8 (x128i, x128h);
  x128i = _mm_mask_cvts_biasph_bf8 (x128i, m8, x128i, x128h);
  x128i = _mm_maskz_cvts_biasph_bf8 (m8, x128i, x128h);

  x128i = _mm256_cvts_biasph_bf8 (x256i, x256h);
  x128i = _mm256_mask_cvts_biasph_bf8 (x128i, m16, x256i, x256h);
  x128i = _mm256_maskz_cvts_biasph_bf8 (m16, x256i, x256h);
}

void extern
avx10_2_vcvtbiasph2hf8_test (void)
{
  x128i = _mm_cvtbiasph_hf8 (x128i, x128h);
  x128i = _mm_mask_cvtbiasph_hf8 (x128i, m8, x128i, x128h);
  x128i = _mm_maskz_cvtbiasph_hf8 (m8, x128i, x128h);

  x128i = _mm256_cvtbiasph_hf8 (x256i, x256h);
  x128i = _mm256_mask_cvtbiasph_hf8 (x128i, m16, x256i, x256h);
  x128i = _mm256_maskz_cvtbiasph_hf8 (m16, x256i, x256h);
}

void extern
avx10_2_vcvtbiasph2hf8s_test (void)
{
  x128i = _mm_cvts_biasph_hf8 (x128i, x128h);
  x128i = _mm_mask_cvts_biasph_hf8 (x128i, m8, x128i, x128h);
  x128i = _mm_maskz_cvts_biasph_hf8 (m8, x128i, x128h);

  x128i = _mm256_cvts_biasph_hf8 (x256i, x256h);
  x128i = _mm256_mask_cvts_biasph_hf8 (x128i, m16, x256i, x256h);
  x128i = _mm256_maskz_cvts_biasph_hf8 (m16, x256i, x256h);
}

void extern
avx10_2_vcvt2ph2bf8_test (void)
{
  x128i = _mm_cvt2ph_bf8 (x128h, x128h);
  x128i = _mm_mask_cvt2ph_bf8 (x128i, m16, x128h, x128h);
  x128i = _mm_maskz_cvt2ph_bf8 (m16, x128h, x128h);
  x256i = _mm256_cvt2ph_bf8 (x256h, x256h);
  x256i = _mm256_mask_cvt2ph_bf8 (x256i, m32, x256h, x256h);
  x256i = _mm256_maskz_cvt2ph_bf8 (m32, x256h, x256h);
}

void extern
avx10_2_vcvt2ph2bf8s_test (void)
{
  x128i = _mm_cvts_2ph_bf8 (x128h, x128h);
  x128i = _mm_mask_cvts_2ph_bf8 (x128i, m16, x128h, x128h);
  x128i = _mm_maskz_cvts_2ph_bf8 (m16, x128h, x128h);
  x256i = _mm256_cvts_2ph_bf8 (x256h, x256h);
  x256i = _mm256_mask_cvts_2ph_bf8 (x256i, m32, x256h, x256h);
  x256i = _mm256_maskz_cvts_2ph_bf8 (m32, x256h, x256h);
}

void extern
avx10_2_vcvt2ph2hf8_test (void)
{
  x128i = _mm_cvt2ph_hf8 (x128h, x128h);
  x128i = _mm_mask_cvt2ph_hf8 (x128i, m16, x128h, x128h);
  x128i = _mm_maskz_cvt2ph_hf8 (m16, x128h, x128h);
  x256i = _mm256_cvt2ph_hf8 (x256h, x256h);
  x256i = _mm256_mask_cvt2ph_hf8 (x256i, m32, x256h, x256h);
  x256i = _mm256_maskz_cvt2ph_hf8 (m32, x256h, x256h);
}

void extern
avx10_2_vcvt2ph2hf8s_test (void)
{
  x128i = _mm_cvts_2ph_hf8 (x128h, x128h);
  x128i = _mm_mask_cvts_2ph_hf8 (x128i, m16, x128h, x128h);
  x128i = _mm_maskz_cvts_2ph_hf8 (m16, x128h, x128h);
  x256i = _mm256_cvts_2ph_hf8 (x256h, x256h);
  x256i = _mm256_mask_cvts_2ph_hf8 (x256i, m32, x256h, x256h);
  x256i = _mm256_maskz_cvts_2ph_hf8 (m32, x256h, x256h);
}

void extern
avx10_2_vcvthf82ph_test (void)
{
  x128h = _mm_cvthf8_ph (x128i);
  x128h = _mm_mask_cvthf8_ph (x128h, m8, x128i);
  x128h = _mm_maskz_cvthf8_ph (m8, x128i);

  x256h = _mm256_cvthf8_ph (x128i);
  x256h = _mm256_mask_cvthf8_ph (x256h, m16, x128i);
  x256h = _mm256_maskz_cvthf8_ph (m16, x128i);
}

void extern
avx10_2_vcvtph2bf8_test (void)
{
  x128i = _mm_cvtph_bf8 (x128h);
  x128i = _mm_mask_cvtph_bf8 (x128i, m8, x128h);
  x128i = _mm_maskz_cvtph_bf8 (m8, x128h);

  x128i = _mm256_cvtph_bf8 (x256h);
  x128i = _mm256_mask_cvtph_bf8 (x128i, m16, x256h);
  x128i = _mm256_maskz_cvtph_bf8 (m16, x256h);
}

void extern
avx10_2_vcvtph2bf8s_test (void)
{
  x128i = _mm_cvts_ph_bf8 (x128h);
  x128i = _mm_mask_cvts_ph_bf8 (x128i, m8, x128h);
  x128i = _mm_maskz_cvts_ph_bf8 (m8, x128h);

  x128i = _mm256_cvts_ph_bf8 (x256h);
  x128i = _mm256_mask_cvts_ph_bf8 (x128i, m16, x256h);
  x128i = _mm256_maskz_cvts_ph_bf8 (m16, x256h);
}

void extern
avx10_2_vcvtph2hf8_test (void)
{
  x128i = _mm_cvtph_hf8 (x128h);
  x128i = _mm_mask_cvtph_hf8 (x128i, m8, x128h);
  x128i = _mm_maskz_cvtph_hf8 (m8, x128h);

  x128i = _mm256_cvtph_hf8 (x256h);
  x128i = _mm256_mask_cvtph_hf8 (x128i, m16, x256h);
  x128i = _mm256_maskz_cvtph_hf8 (m16, x256h);
}

void extern
avx10_2_vcvtph2hf8s_test (void)
{
  x128i = _mm_cvts_ph_hf8 (x128h);
  x128i = _mm_mask_cvts_ph_hf8 (x128i, m8, x128h);
  x128i = _mm_maskz_cvts_ph_hf8 (m8, x128h);

  x128i = _mm256_cvts_ph_hf8 (x256h);
  x128i = _mm256_mask_cvts_ph_hf8 (x128i, m16, x256h);
  x128i = _mm256_maskz_cvts_ph_hf8 (m16, x256h);
}

void extern
avx10_2_cvtbf8_fp16_test (void)
{
  y = _mm_cvtbf8_ph (z3);
  y = _mm_mask_cvtbf8_ph (z, m8, z3);
  y = _mm_maskz_cvtbf8_ph (m8, z3);

  y2 = _mm256_cvtbf8_ph (z3);
  y2 = _mm256_mask_cvtbf8_ph (z2, m16, z3);
  y2 = _mm256_maskz_cvtbf8_ph (m16, z3);
}
