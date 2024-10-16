/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-final { scan-assembler-times "vaddpd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vaddpd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vaddpd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vaddph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vaddph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vaddph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vaddps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vaddps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vaddps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcmppd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcmppd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcmpph\[ \\t\]+\\\$3\[^\n\r]*\{sae\}\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcmpph\[ \\t\]+\[^\{\n\]*\\\$4\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%k\[0-9\]\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcmpps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcmpps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtdq2phy\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtdq2ph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtdq2ph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtdq2ps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtdq2ps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtdq2ps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2phy\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2ph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2ph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2ps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2ps\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2ps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2dq\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2dq\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2dq\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2qq\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2qq\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2qq\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2udq\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2udq\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2udq\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\n\]*%ymm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2dq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2dq\[ \\t\]+\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2dq\[ \\t\]+\{rz-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2pd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2pd\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2pd\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2ps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2ps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2ps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2psx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2psx\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2psx\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2qq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2qq\[ \\t\]+\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2qq\[ \\t\]+\{rz-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2udq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2udq\[ \\t\]+\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2udq\[ \\t\]+\{rz-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2uqq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2uqq\[ \\t\]+\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2uqq\[ \\t\]+\{rz-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2uw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2uw\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2uw\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2w\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2w\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2w\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2pd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2pd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2pd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2phxy\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2phx\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2phx\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2dq\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2dq\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2dq\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2qq\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2qq\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2qq\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2udq\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2udq\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2udq\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2uqq\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2uqq\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2uqq\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */

#include <immintrin.h>

volatile __m128 hx;
volatile __m128i hxi;
volatile __m128h hxh;
volatile __m256 x;
volatile __m256d xd;
volatile __m256h xh;
volatile __m256i xi;
volatile __mmask8 m8;
volatile __mmask16 m16;
volatile __mmask32 m32;

void extern
avx10_2_test_1 (void)
{
  xd = _mm256_add_round_pd (xd, xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_add_round_pd (xd, m8, xd, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_add_round_pd (m8, xd, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xh = _mm256_add_round_ph (xh, xh, 8);
  xh = _mm256_mask_add_round_ph (xh, m32, xh, xh, 8);
  xh = _mm256_maskz_add_round_ph (m32, xh, xh, 11);

  x = _mm256_add_round_ps (x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_add_round_ps (x, m16, x, x, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_add_round_ps (m16, x, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_2 (void)
{
  m8 = _mm256_cmp_round_pd_mask (xd, xd, _CMP_FALSE_OQ, _MM_FROUND_NO_EXC);
  m8 = _mm256_mask_cmp_round_pd_mask (m8, xd, xd, _CMP_FALSE_OQ, _MM_FROUND_NO_EXC);

  m16 = _mm256_cmp_round_ph_mask (xh, xh, 3, 8);
  m16 = _mm256_mask_cmp_round_ph_mask (m16, xh, xh, 4, 4);

  m8 = _mm256_cmp_round_ps_mask (x, x, _CMP_FALSE_OQ, _MM_FROUND_NO_EXC);
  m8 = _mm256_mask_cmp_round_ps_mask (m8, x, x, _CMP_FALSE_OQ, _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_3 (void)
{
  hxh = _mm256_cvt_roundepi32_ph (xi, 4);
  hxh = _mm256_mask_cvt_roundepi32_ph (hxh, m8, xi, 8);
  hxh = _mm256_maskz_cvt_roundepi32_ph (m8, xi, 11);

  x = _mm256_cvt_roundepi32_ps (xi, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_cvt_roundepi32_ps (x, m8, xi, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_cvt_roundepi32_ps (m8, xi, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_4 (void)
{
  hxh = _mm256_cvt_roundpd_ph (xd, 4);
  hxh = _mm256_mask_cvt_roundpd_ph (hxh, m8, xd, 8);
  hxh = _mm256_maskz_cvt_roundpd_ph (m8, xd, 11);

  hx = _mm256_cvt_roundpd_ps (xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  hx = _mm256_mask_cvt_roundpd_ps (hx, 4, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  hx = _mm256_maskz_cvt_roundpd_ps (6, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_5 (void)
{
  hxi = _mm256_cvt_roundpd_epi32 (xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  hxi = _mm256_mask_cvt_roundpd_epi32 (hxi, m8, xd, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  hxi = _mm256_maskz_cvt_roundpd_epi32 (m8, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xi = _mm256_cvt_roundpd_epi64 (xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xi = _mm256_mask_cvt_roundpd_epi64 (xi, m8, xd, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xi = _mm256_maskz_cvt_roundpd_epi64 (m8, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  hxi = _mm256_cvt_roundpd_epu32 (xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  hxi = _mm256_mask_cvt_roundpd_epu32 (hxi, m8, xd, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  hxi = _mm256_maskz_cvt_roundpd_epu32 (m8, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xi = _mm256_cvt_roundpd_epu64 (xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xi = _mm256_mask_cvt_roundpd_epu64 (xi, m8, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xi = _mm256_maskz_cvt_roundpd_epu64 (m8, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_6 (void)
{
  xi = _mm256_cvt_roundph_epi32 (hxh, 4);
  xi = _mm256_mask_cvt_roundph_epi32 (xi, m8, hxh, 8);
  xi = _mm256_maskz_cvt_roundph_epi32 (m8, hxh, 11);

  xi = _mm256_cvt_roundph_epi64 (hxh, 4);
  xi = _mm256_mask_cvt_roundph_epi64 (xi, m8, hxh, 8);
  xi = _mm256_maskz_cvt_roundph_epi64 (m8, hxh, 11);

  xi = _mm256_cvt_roundph_epu32 (hxh, 4);
  xi = _mm256_mask_cvt_roundph_epu32 (xi, m8, hxh, 8);
  xi = _mm256_maskz_cvt_roundph_epu32 (m8, hxh, 11);

  xi = _mm256_cvt_roundph_epu64 (hxh, 4);
  xi = _mm256_mask_cvt_roundph_epu64 (xi, m8, hxh, 8);
  xi = _mm256_maskz_cvt_roundph_epu64 (m8, hxh, 11);
}

void extern
avx10_2_test_7 (void)
{
  xd = _mm256_cvt_roundph_pd (hxh, 4);
  xd = _mm256_mask_cvt_roundph_pd (xd, m8, hxh, 8);
  xd = _mm256_maskz_cvt_roundph_pd (m8, hxh, 8);

  x = _mm256_cvt_roundph_ps (hxh, _MM_FROUND_NO_EXC);
  x = _mm256_mask_cvt_roundph_ps (x, 4, hxh, _MM_FROUND_NO_EXC);
  x = _mm256_maskz_cvt_roundph_ps (6, hxh, _MM_FROUND_NO_EXC);

  x = _mm256_cvtx_roundph_ps (hxh, 4);
  x = _mm256_mask_cvtx_roundph_ps (x, m8, hxh, 8);
  x = _mm256_maskz_cvtx_roundph_ps (m8, hxh, 8);
}

void extern
avx10_2_test_8 (void)
{
  xi = _mm256_cvt_roundph_epu16 (xh, 4);
  xi = _mm256_mask_cvt_roundph_epu16 (xi, m16, xh, 8);
  xi = _mm256_maskz_cvt_roundph_epu16 (m16, xh, 11);

  xi = _mm256_cvt_roundph_epi16 (xh, 4);
  xi = _mm256_mask_cvt_roundph_epi16 (xi, m16, xh, 8);
  xi = _mm256_maskz_cvt_roundph_epi16 (m16, xh, 11);
}

void extern
avx10_2_test_9 (void)
{
  xd = _mm256_cvt_roundps_pd (hx, _MM_FROUND_NO_EXC);
  xd = _mm256_mask_cvt_roundps_pd (xd, m8, hx, _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_cvt_roundps_pd (m8, hx, _MM_FROUND_NO_EXC);

  hxh = _mm256_cvtx_roundps_ph (x, 4);
  hxh = _mm256_mask_cvtx_roundps_ph (hxh, m8, x, 8);
  hxh = _mm256_maskz_cvtx_roundps_ph (m8, x, 11);
}

void extern
avx10_2_test_10 (void)
{
  xi = _mm256_cvt_roundps_epi32 (x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xi = _mm256_mask_cvt_roundps_epi32 (xi, m8, x, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xi = _mm256_maskz_cvt_roundps_epi32 (m8, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xi = _mm256_cvt_roundps_epi64 (hx, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xi = _mm256_mask_cvt_roundps_epi64 (xi, m8, hx, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xi = _mm256_maskz_cvt_roundps_epi64 (m8, hx, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xi = _mm256_cvt_roundps_epu32 (x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xi = _mm256_mask_cvt_roundps_epu32 (xi, m8, x, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xi = _mm256_maskz_cvt_roundps_epu32 (m8, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xi = _mm256_cvt_roundps_epu64 (hx, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xi = _mm256_mask_cvt_roundps_epu64 (xi, m8, hx, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xi = _mm256_maskz_cvt_roundps_epu64 (m8, hx, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}
