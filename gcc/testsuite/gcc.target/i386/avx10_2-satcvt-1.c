/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-final { scan-assembler-times "vcvtph2ibs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2ibs\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2ibs\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2iubs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2iubs\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2iubs\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2ibs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2ibs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2ibs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2iubs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2iubs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2iubs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2ibs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2ibs\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2ibs\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2iubs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2iubs\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2iubs\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2ibs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2ibs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2ibs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2iubs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2iubs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2iubs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162ibs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162ibs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162ibs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162iubs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162iubs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162iubs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162ibs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162ibs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162ibs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162iubs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162iubs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162iubs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtnebf162iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162ibs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttnebf162iubs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dqsy\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dqsy\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dqsy\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udqsy\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udqsy\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udqsy\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dqsx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dqsx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dqsx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udqsx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udqsx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udqsx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqqs\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttsd2sis\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%e.x+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttsd2usis\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%e.x+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttss2sis\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%e.x+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttss2usis\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%e.x+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttsd2sis\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%r.x+(?:\n|\[ \\t\]+#)" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvttsd2usis\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%r.x+(?:\n|\[ \\t\]+#)" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvttss2sis\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%r.x+(?:\n|\[ \\t\]+#)" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvttss2usis\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%r.x+(?:\n|\[ \\t\]+#)" 1 { target { ! ia32 } } } } */

#include <immintrin.h>

volatile __m128 hx;
volatile __m128i hxi;
volatile __m128h hxh;
volatile __m128d hxd;
volatile __m128bh hxbh;
volatile __m256 x;
volatile __m256h xh;
volatile __m256i xi;
volatile __m256d xd;
volatile __m256bh xbh;
volatile __mmask8 m8;
volatile __mmask16 m16;
volatile int i;
volatile unsigned int ui;
volatile long long ll;
volatile unsigned long long ull;

void extern
avx10_2_test (void)
{
  xi = _mm256_ipcvt_roundph_epi16 (xh, 4);
  xi = _mm256_mask_ipcvt_roundph_epi16 (xi, m16, xh, 8);
  xi = _mm256_maskz_ipcvt_roundph_epi16 (m16, xh, 11);

  xi = _mm256_ipcvt_roundph_epu16 (xh, 4);
  xi = _mm256_mask_ipcvt_roundph_epu16 (xi, m16, xh, 8);
  xi = _mm256_maskz_ipcvt_roundph_epu16 (m16, xh, 11);

  xi = _mm256_ipcvtt_roundph_epi16 (xh, 4);
  xi = _mm256_mask_ipcvtt_roundph_epi16 (xi, m16, xh, 8);
  xi = _mm256_maskz_ipcvtt_roundph_epi16 (m16, xh, 8);

  xi = _mm256_ipcvtt_roundph_epu16 (xh, 4);
  xi = _mm256_mask_ipcvtt_roundph_epu16 (xi, m16, xh, 8);
  xi = _mm256_maskz_ipcvtt_roundph_epu16 (m16, xh, 8);

  xi = _mm256_ipcvt_roundps_epi32 (x, 4);
  xi = _mm256_mask_ipcvt_roundps_epi32 (xi, m8, x, 8);
  xi = _mm256_maskz_ipcvt_roundps_epi32 (m8, x, 11);

  xi = _mm256_ipcvt_roundps_epu32 (x, 4);
  xi = _mm256_mask_ipcvt_roundps_epu32 (xi, m8, x, 8);
  xi = _mm256_maskz_ipcvt_roundps_epu32 (m8, x, 11);

  xi = _mm256_ipcvtt_roundps_epi32 (x, 4);
  xi = _mm256_mask_ipcvtt_roundps_epi32 (xi, m8, x, 8);
  xi = _mm256_maskz_ipcvtt_roundps_epi32 (m8, x, 8);

  xi = _mm256_ipcvtt_roundps_epu32 (x, 4);
  xi = _mm256_mask_ipcvtt_roundps_epu32 (xi, m8, x, 8);
  xi = _mm256_maskz_ipcvtt_roundps_epu32 (m8, x, 8);

  xi = _mm256_ipcvtnebf16_epi16 (xbh);
  xi = _mm256_mask_ipcvtnebf16_epi16 (xi, m16, xbh);
  xi = _mm256_maskz_ipcvtnebf16_epi16 (m16, xbh);

  xi = _mm256_ipcvtnebf16_epu16 (xbh);
  xi = _mm256_mask_ipcvtnebf16_epu16 (xi, m16, xbh);
  xi = _mm256_maskz_ipcvtnebf16_epu16 (m16, xbh);

  xi = _mm256_ipcvttnebf16_epi16 (xbh);
  xi = _mm256_mask_ipcvttnebf16_epi16 (xi, m16, xbh);
  xi = _mm256_maskz_ipcvttnebf16_epi16 (m16, xbh);

  xi = _mm256_ipcvttnebf16_epu16 (xbh);
  xi = _mm256_mask_ipcvttnebf16_epu16 (xi, m16, xbh);
  xi = _mm256_maskz_ipcvttnebf16_epu16 (m16, xbh);

  hxi = _mm_ipcvtph_epi16 (hxh);
  hxi = _mm_mask_ipcvtph_epi16 (hxi, m8, hxh);
  hxi = _mm_maskz_ipcvtph_epi16 (m8, hxh);

  hxi = _mm_ipcvtph_epu16 (hxh);
  hxi = _mm_mask_ipcvtph_epu16 (hxi, m8, hxh);
  hxi = _mm_maskz_ipcvtph_epu16 (m8, hxh);

  hxi = _mm_ipcvttph_epi16 (hxh);
  hxi = _mm_mask_ipcvttph_epi16 (hxi, m8, hxh);
  hxi = _mm_maskz_ipcvttph_epi16 (m8, hxh);

  hxi = _mm_ipcvttph_epu16 (hxh);
  hxi = _mm_mask_ipcvttph_epu16 (hxi, m8, hxh);
  hxi = _mm_maskz_ipcvttph_epu16 (m8, hxh);

  hxi = _mm_ipcvtps_epi32 (hx);
  hxi = _mm_mask_ipcvtps_epi32 (hxi, m8, hx);
  hxi = _mm_maskz_ipcvtps_epi32 (m8, hx);

  hxi = _mm_ipcvtps_epu32 (hx);
  hxi = _mm_mask_ipcvtps_epu32 (hxi, m8, hx);
  hxi = _mm_maskz_ipcvtps_epu32 (m8, hx);

  hxi = _mm_ipcvttps_epi32 (hx);
  hxi = _mm_mask_ipcvttps_epi32 (hxi, m8, hx);
  hxi = _mm_maskz_ipcvttps_epi32 (m8, hx);

  hxi = _mm_ipcvttps_epu32 (hx);
  hxi = _mm_mask_ipcvttps_epu32 (hxi, m8, hx);
  hxi = _mm_maskz_ipcvttps_epu32 (m8, hx);

  hxi = _mm_ipcvtnebf16_epi16 (hxbh);
  hxi = _mm_mask_ipcvtnebf16_epi16 (hxi, m8, hxbh);
  hxi = _mm_maskz_ipcvtnebf16_epi16 (m8, hxbh);

  hxi = _mm_ipcvtnebf16_epu16 (hxbh);
  hxi = _mm_mask_ipcvtnebf16_epu16 (hxi, m8, hxbh);
  hxi = _mm_maskz_ipcvtnebf16_epu16 (m8, hxbh);

  hxi = _mm_ipcvttnebf16_epi16 (hxbh);
  hxi = _mm_mask_ipcvttnebf16_epi16 (hxi, m8, hxbh);
  hxi = _mm_maskz_ipcvttnebf16_epi16 (m8, hxbh);

  hxi = _mm_ipcvttnebf16_epu16 (hxbh);
  hxi = _mm_mask_ipcvttnebf16_epu16 (hxi, m8, hxbh);
  hxi = _mm_maskz_ipcvttnebf16_epu16 (m8, hxbh);

  hxi = _mm256_cvtts_roundpd_epi32 (xd, 8);
  hxi = _mm256_mask_cvtts_roundpd_epi32 (hxi, m8, xd, 8);
  hxi = _mm256_maskz_cvtts_roundpd_epi32 (m8, xd, 8);

  xi = _mm256_cvtts_roundpd_epi64 (xd, 8);
  xi = _mm256_mask_cvtts_roundpd_epi64 (xi, m8, xd, 8);
  xi = _mm256_maskz_cvtts_roundpd_epi64 (m8, xd, 8);

  hxi = _mm256_cvtts_roundpd_epu32 (xd, 8);
  hxi = _mm256_mask_cvtts_roundpd_epu32 (hxi, m8, xd, 8);
  hxi = _mm256_maskz_cvtts_roundpd_epu32 (m8, xd, 8);

  xi = _mm256_cvtts_roundpd_epu64 (xd, 8);
  xi = _mm256_mask_cvtts_roundpd_epu64 (xi, m8, xd, 8);
  xi = _mm256_maskz_cvtts_roundpd_epu64 (m8, xd, 8);

  xi = _mm256_cvtts_roundps_epi32 (x, 8);
  xi = _mm256_mask_cvtts_roundps_epi32 (xi, m16, x, 8);
  xi = _mm256_maskz_cvtts_roundps_epi32 (m16, x, 8);

  xi = _mm256_cvtts_roundps_epi64 (hx, 8);
  xi = _mm256_mask_cvtts_roundps_epi64 (xi, m8, hx, 8);
  xi = _mm256_maskz_cvtts_roundps_epi64 (m8, hx, 8);

  xi = _mm256_cvtts_roundps_epu32 (x, 8);
  xi = _mm256_mask_cvtts_roundps_epu32 (xi, m16, x, 8);
  xi = _mm256_maskz_cvtts_roundps_epu32 (m16, x, 8);

  xi = _mm256_cvtts_roundps_epu64 (hx, 8);
  xi = _mm256_mask_cvtts_roundps_epu64 (xi, m8, hx, 8);
  xi = _mm256_maskz_cvtts_roundps_epu64 (m8, hx, 8);

  hxi = _mm_cvttspd_epi32 (hxd);
  hxi = _mm_mask_cvttspd_epi32 (hxi, m8, hxd);
  hxi = _mm_maskz_cvttspd_epi32 (m8, hxd);

  hxi = _mm_cvttspd_epi64 (hxd);
  hxi = _mm_mask_cvttspd_epi64 (hxi, m8, hxd);
  hxi = _mm_maskz_cvttspd_epi64 (m8, hxd);

  hxi = _mm_cvttspd_epu32 (hxd);
  hxi = _mm_mask_cvttspd_epu32 (hxi, m8, hxd);
  hxi = _mm_maskz_cvttspd_epu32 (m8, hxd);

  hxi = _mm_cvttspd_epu64 (hxd);
  hxi = _mm_mask_cvttspd_epu64 (hxi, m8, hxd);
  hxi = _mm_maskz_cvttspd_epu64 (m8, hxd);

  hxi = _mm_cvttsps_epi32 (hx);
  hxi = _mm_mask_cvttsps_epi32 (hxi, m8, hx);
  hxi = _mm_maskz_cvttsps_epi32 (m8, hx);

  hxi = _mm_cvttsps_epi64 (hx);
  hxi = _mm_mask_cvttsps_epi64 (hxi, m8, hx);
  hxi = _mm_maskz_cvttsps_epi64 (m8, hx);

  hxi = _mm_cvttsps_epu32 (hx);
  hxi = _mm_mask_cvttsps_epu32 (hxi, m8, hx);
  hxi = _mm_maskz_cvttsps_epu32 (m8, hx);

  hxi = _mm_cvttsps_epu64 (hx);
  hxi = _mm_mask_cvttsps_epu64 (hxi, m8, hx);
  hxi = _mm_maskz_cvttsps_epu64 (m8, hx);

  i = _mm_cvtts_roundsd_epi32 (hxd, 8);
  ui = _mm_cvtts_roundsd_epu32 (hxd, 8);
  i = _mm_cvtts_roundss_epi32 (hx, 8);
  ui = _mm_cvtts_roundss_epu32 (hx, 8);

#ifdef __x86_64__
  ll = _mm_cvtts_roundsd_epi64 (hxd, 8);
  ull = _mm_cvtts_roundsd_epu64 (hxd, 8);
  ll = _mm_cvtts_roundss_epi64 (hx, 8);
  ull = _mm_cvtts_roundss_epu64 (hx, 8);
#endif
}
