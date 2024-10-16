/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2 -O2" } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvt2ps2phx\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
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
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2bf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ph2hf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ph\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ph\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ph\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ph\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ph\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ph\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8x\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8x\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8x\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8y\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8y\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8y\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8sx\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8sx\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8sx\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8sy\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8sy\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2bf8sy\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8x\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8x\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8x\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8y\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8y\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8y\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8sx\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8sx\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8sx\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8sy\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8sy\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneph2hf8sy\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
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

  y2 = _mm256_cvtx_round2ps_ph (a2, b2, 8);
  y2 = _mm256_mask_cvtx_round2ps_ph (y2, m16, a2, b2, 8);
  y2 = _mm256_maskz_cvtx_round2ps_ph (m16, a2, b2, 8);
}

void extern
avx10_2_vcvtbiasph2bf8_test (void)
{
  x128i = _mm_cvtbiasph_pbf8 (x128i, x128h);
  x128i = _mm_mask_cvtbiasph_pbf8 (x128i, m8, x128i, x128h);
  x128i = _mm_maskz_cvtbiasph_pbf8 (m8, x128i, x128h);

  x128i = _mm256_cvtbiasph_pbf8 (x256i, x256h);
  x128i = _mm256_mask_cvtbiasph_pbf8 (x128i, m16, x256i, x256h);
  x128i = _mm256_maskz_cvtbiasph_pbf8 (m16, x256i, x256h);
}

void extern
avx10_2_vcvtbiasph2bf8s_test (void)
{
  x128i = _mm_cvtbiassph_pbf8 (x128i, x128h);
  x128i = _mm_mask_cvtbiassph_pbf8 (x128i, m8, x128i, x128h);
  x128i = _mm_maskz_cvtbiassph_pbf8 (m8, x128i, x128h);

  x128i = _mm256_cvtbiassph_pbf8 (x256i, x256h);
  x128i = _mm256_mask_cvtbiassph_pbf8 (x128i, m16, x256i, x256h);
  x128i = _mm256_maskz_cvtbiassph_pbf8 (m16, x256i, x256h);
}

void extern
avx10_2_vcvtbiasph2hf8_test (void)
{
  x128i = _mm_cvtbiasph_phf8 (x128i, x128h);
  x128i = _mm_mask_cvtbiasph_phf8 (x128i, m8, x128i, x128h);
  x128i = _mm_maskz_cvtbiasph_phf8 (m8, x128i, x128h);

  x128i = _mm256_cvtbiasph_phf8 (x256i, x256h);
  x128i = _mm256_mask_cvtbiasph_phf8 (x128i, m16, x256i, x256h);
  x128i = _mm256_maskz_cvtbiasph_phf8 (m16, x256i, x256h);
}

void extern
avx10_2_vcvtbiasph2hf8s_test (void)
{
  x128i = _mm_cvtbiassph_phf8 (x128i, x128h);
  x128i = _mm_mask_cvtbiassph_phf8 (x128i, m8, x128i, x128h);
  x128i = _mm_maskz_cvtbiassph_phf8 (m8, x128i, x128h);

  x128i = _mm256_cvtbiassph_phf8 (x256i, x256h);
  x128i = _mm256_mask_cvtbiassph_phf8 (x128i, m16, x256i, x256h);
  x128i = _mm256_maskz_cvtbiassph_phf8 (m16, x256i, x256h);
}

void extern
avx10_2_vcvtne2ph2bf8_test (void)
{
  x128i = _mm_cvtne2ph_pbf8 (x128h, x128h);
  x128i = _mm_mask_cvtne2ph_pbf8 (x128i, m16, x128h, x128h);
  x128i = _mm_maskz_cvtne2ph_pbf8 (m16, x128h, x128h);
  x256i = _mm256_cvtne2ph_pbf8 (x256h, x256h);
  x256i = _mm256_mask_cvtne2ph_pbf8 (x256i, m32, x256h, x256h);
  x256i = _mm256_maskz_cvtne2ph_pbf8 (m32, x256h, x256h);
}

void extern
avx10_2_vcvtne2ph2bf8s_test (void)
{
  x128i = _mm_cvtnes2ph_pbf8 (x128h, x128h);
  x128i = _mm_mask_cvtnes2ph_pbf8 (x128i, m16, x128h, x128h);
  x128i = _mm_maskz_cvtnes2ph_pbf8 (m16, x128h, x128h);
  x256i = _mm256_cvtnes2ph_pbf8 (x256h, x256h);
  x256i = _mm256_mask_cvtnes2ph_pbf8 (x256i, m32, x256h, x256h);
  x256i = _mm256_maskz_cvtnes2ph_pbf8 (m32, x256h, x256h);
}

void extern
avx10_2_vcvtne2ph2hf8_test (void)
{
  x128i = _mm_cvtne2ph_phf8 (x128h, x128h);
  x128i = _mm_mask_cvtne2ph_phf8 (x128i, m16, x128h, x128h);
  x128i = _mm_maskz_cvtne2ph_phf8 (m16, x128h, x128h);
  x256i = _mm256_cvtne2ph_phf8 (x256h, x256h);
  x256i = _mm256_mask_cvtne2ph_phf8 (x256i, m32, x256h, x256h);
  x256i = _mm256_maskz_cvtne2ph_phf8 (m32, x256h, x256h);
}

void extern
avx10_2_vcvtne2ph2hf8s_test (void)
{
  x128i = _mm_cvtnes2ph_phf8 (x128h, x128h);
  x128i = _mm_mask_cvtnes2ph_phf8 (x128i, m16, x128h, x128h);
  x128i = _mm_maskz_cvtnes2ph_phf8 (m16, x128h, x128h);
  x256i = _mm256_cvtnes2ph_phf8 (x256h, x256h);
  x256i = _mm256_mask_cvtnes2ph_phf8 (x256i, m32, x256h, x256h);
  x256i = _mm256_maskz_cvtnes2ph_phf8 (m32, x256h, x256h);
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
avx10_2_vcvtneph2bf8_test (void)
{
  x128i = _mm_cvtneph_pbf8 (x128h);
  x128i = _mm_mask_cvtneph_pbf8 (x128i, m8, x128h);
  x128i = _mm_maskz_cvtneph_pbf8 (m8, x128h);

  x128i = _mm256_cvtneph_pbf8 (x256h);
  x128i = _mm256_mask_cvtneph_pbf8 (x128i, m16, x256h);
  x128i = _mm256_maskz_cvtneph_pbf8 (m16, x256h);
}

void extern
avx10_2_vcvtneph2bf8s_test (void)
{
  x128i = _mm_cvtnesph_pbf8 (x128h);
  x128i = _mm_mask_cvtnesph_pbf8 (x128i, m8, x128h);
  x128i = _mm_maskz_cvtnesph_pbf8 (m8, x128h);

  x128i = _mm256_cvtnesph_pbf8 (x256h);
  x128i = _mm256_mask_cvtnesph_pbf8 (x128i, m16, x256h);
  x128i = _mm256_maskz_cvtnesph_pbf8 (m16, x256h);
}

void extern
avx10_2_vcvtneph2hf8_test (void)
{
  x128i = _mm_cvtneph_phf8 (x128h);
  x128i = _mm_mask_cvtneph_phf8 (x128i, m8, x128h);
  x128i = _mm_maskz_cvtneph_phf8 (m8, x128h);

  x128i = _mm256_cvtneph_phf8 (x256h);
  x128i = _mm256_mask_cvtneph_phf8 (x128i, m16, x256h);
  x128i = _mm256_maskz_cvtneph_phf8 (m16, x256h);
}

void extern
avx10_2_vcvtneph2hf8s_test (void)
{
  x128i = _mm_cvtnesph_phf8 (x128h);
  x128i = _mm_mask_cvtnesph_phf8 (x128i, m8, x128h);
  x128i = _mm_maskz_cvtnesph_phf8 (m8, x128h);

  x128i = _mm256_cvtnesph_phf8 (x256h);
  x128i = _mm256_mask_cvtnesph_phf8 (x128i, m16, x256h);
  x128i = _mm256_maskz_cvtnesph_phf8 (m16, x256h);
}

void extern
avx10_2_cvtbf8_fp16_test (void)
{
  y = _mm_cvtpbf8_ph (z3);
  y = _mm_mask_cvtpbf8_ph (z, m8, z3);
  y = _mm_maskz_cvtpbf8_ph (m8, z3);

  y2 = _mm256_cvtpbf8_ph (z3);
  y2 = _mm256_mask_cvtpbf8_ph (z2, m8, z3);
  y2 = _mm256_maskz_cvtpbf8_ph (m8, z3);
}
