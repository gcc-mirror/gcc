/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-final { scan-assembler-times "vcvtph2ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vcvtph2ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2ibs\[ \\t\]+\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2ibs\[ \\t\]+\{rz-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vcvtph2iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2iubs\[ \\t\]+\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtph2iubs\[ \\t\]+\{rz-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vcvttph2ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2ibs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2ibs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vcvttph2iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2iubs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttph2iubs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vcvtps2ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2ibs\[ \\t\]+\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2ibs\[ \\t\]+\{rz-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vcvtps2iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2iubs\[ \\t\]+\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtps2iubs\[ \\t\]+\{rz-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vcvttps2ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2ibs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2ibs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vcvttps2iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2iubs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2iubs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtbf162ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtbf162ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtbf162ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtbf162iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtbf162iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtbf162iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttbf162ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttbf162ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttbf162ibs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttbf162iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttbf162iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttbf162iubs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2dqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2udqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttpd2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2dqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qqs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qqs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qqs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2qqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udqs\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2udqs\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqqs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqqs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqqs\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvttps2uqqs\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */

#include <immintrin.h>

volatile __m256 hx;
volatile __m256i hxi;
volatile __m512 x;
volatile __m512h xh;
volatile __m512i xi;
volatile __m512d xd;
volatile __m512bh xbh;
volatile __mmask8 m8;
volatile __mmask16 m16;
volatile __mmask32 m32;

void extern
avx10_2_test (void)
{
  xi = _mm512_ipcvts_ph_epi8 (xh);
  xi = _mm512_mask_ipcvts_ph_epi8 (xi, m32, xh);
  xi = _mm512_maskz_ipcvts_ph_epi8 (m32, xh);
  xi = _mm512_ipcvts_roundph_epi8 (xh, 4);
  xi = _mm512_mask_ipcvts_roundph_epi8 (xi, m32, xh, 8);
  xi = _mm512_maskz_ipcvts_roundph_epi8 (m32, xh, 11);

  xi = _mm512_ipcvts_ph_epu8 (xh);
  xi = _mm512_mask_ipcvts_ph_epu8 (xi, m32, xh);
  xi = _mm512_maskz_ipcvts_ph_epu8 (m32, xh);
  xi = _mm512_ipcvts_roundph_epu8 (xh, 4);
  xi = _mm512_mask_ipcvts_roundph_epu8 (xi, m32, xh, 8);
  xi = _mm512_maskz_ipcvts_roundph_epu8 (m32, xh, 11);

  xi = _mm512_ipcvtts_ph_epi8 (xh);
  xi = _mm512_mask_ipcvtts_ph_epi8 (xi, m32, xh);
  xi = _mm512_maskz_ipcvtts_ph_epi8 (m32, xh);
  xi = _mm512_ipcvtts_roundph_epi8 (xh, 4);
  xi = _mm512_mask_ipcvtts_roundph_epi8 (xi, m32, xh, 8);
  xi = _mm512_maskz_ipcvtts_roundph_epi8 (m32, xh, 8);

  xi = _mm512_ipcvtts_ph_epu8 (xh);
  xi = _mm512_mask_ipcvtts_ph_epu8 (xi, m32, xh);
  xi = _mm512_maskz_ipcvtts_ph_epu8 (m32, xh);
  xi = _mm512_ipcvtts_roundph_epu8 (xh, 4);
  xi = _mm512_mask_ipcvtts_roundph_epu8 (xi, m32, xh, 8);
  xi = _mm512_maskz_ipcvtts_roundph_epu8 (m32, xh, 8);

  xi = _mm512_ipcvts_ps_epi8 (x);
  xi = _mm512_mask_ipcvts_ps_epi8 (xi, m16, x);
  xi = _mm512_maskz_ipcvts_ps_epi8 (m16, x);
  xi = _mm512_ipcvts_roundps_epi8 (x, 4);
  xi = _mm512_mask_ipcvts_roundps_epi8 (xi, m16, x, 8);
  xi = _mm512_maskz_ipcvts_roundps_epi8 (m16, x, 11);

  xi = _mm512_ipcvts_ps_epu8 (x);
  xi = _mm512_mask_ipcvts_ps_epu8 (xi, m16, x);
  xi = _mm512_maskz_ipcvts_ps_epu8 (m16, x);
  xi = _mm512_ipcvts_roundps_epu8 (x, 4);
  xi = _mm512_mask_ipcvts_roundps_epu8 (xi, m16, x, 8);
  xi = _mm512_maskz_ipcvts_roundps_epu8 (m16, x, 11);

  xi = _mm512_ipcvtts_ps_epi8 (x);
  xi = _mm512_mask_ipcvtts_ps_epi8 (xi, m16, x);
  xi = _mm512_maskz_ipcvtts_ps_epi8 (m16, x);
  xi = _mm512_ipcvtts_roundps_epi8 (x, 4);
  xi = _mm512_mask_ipcvtts_roundps_epi8 (xi, m16, x, 8);
  xi = _mm512_maskz_ipcvtts_roundps_epi8 (m16, x, 8);

  xi = _mm512_ipcvtts_ps_epu8 (x);
  xi = _mm512_mask_ipcvtts_ps_epu8 (xi, m16, x);
  xi = _mm512_maskz_ipcvtts_ps_epu8 (m16, x);
  xi = _mm512_ipcvtts_roundps_epu8 (x, 4);
  xi = _mm512_mask_ipcvtts_roundps_epu8 (xi, m16, x, 8);
  xi = _mm512_maskz_ipcvtts_roundps_epu8 (m16, x, 8);

  xi = _mm512_ipcvts_bf16_epi8 (xbh);
  xi = _mm512_mask_ipcvts_bf16_epi8 (xi, m32, xbh);
  xi = _mm512_maskz_ipcvts_bf16_epi8 (m32, xbh);

  xi = _mm512_ipcvts_bf16_epu8 (xbh);
  xi = _mm512_mask_ipcvts_bf16_epu8 (xi, m32, xbh);
  xi = _mm512_maskz_ipcvts_bf16_epu8 (m32, xbh);

  xi = _mm512_ipcvtts_bf16_epi8 (xbh);
  xi = _mm512_mask_ipcvtts_bf16_epi8 (xi, m32, xbh);
  xi = _mm512_maskz_ipcvtts_bf16_epi8 (m32, xbh);

  xi = _mm512_ipcvtts_bf16_epu8 (xbh);
  xi = _mm512_mask_ipcvtts_bf16_epu8 (xi, m32, xbh);
  xi = _mm512_maskz_ipcvtts_bf16_epu8 (m32, xbh);

  hxi = _mm512_cvtts_pd_epi32 (xd);
  hxi = _mm512_mask_cvtts_pd_epi32 (hxi, m8, xd);
  hxi = _mm512_maskz_cvtts_pd_epi32 (m8, xd);
  hxi = _mm512_cvtts_roundpd_epi32 (xd, 8);
  hxi = _mm512_mask_cvtts_roundpd_epi32 (hxi, m8, xd, 8);
  hxi = _mm512_maskz_cvtts_roundpd_epi32 (m8, xd, 8);

  xi = _mm512_cvtts_pd_epi64 (xd);
  xi = _mm512_mask_cvtts_pd_epi64 (xi, m8, xd);
  xi = _mm512_maskz_cvtts_pd_epi64 (m8, xd);
  xi = _mm512_cvtts_roundpd_epi64 (xd, 8);
  xi = _mm512_mask_cvtts_roundpd_epi64 (xi, m8, xd, 8);
  xi = _mm512_maskz_cvtts_roundpd_epi64 (m8, xd, 8);

  hxi = _mm512_cvtts_pd_epu32 (xd);
  hxi = _mm512_mask_cvtts_pd_epu32 (hxi, m8, xd);
  hxi = _mm512_maskz_cvtts_pd_epu32 (m8, xd);
  hxi = _mm512_cvtts_roundpd_epu32 (xd, 8);
  hxi = _mm512_mask_cvtts_roundpd_epu32 (hxi, m8, xd, 8);
  hxi = _mm512_maskz_cvtts_roundpd_epu32 (m8, xd, 8);

  xi = _mm512_cvtts_pd_epu64 (xd);
  xi = _mm512_mask_cvtts_pd_epu64 (xi, m8, xd);
  xi = _mm512_maskz_cvtts_pd_epu64 (m8, xd);
  xi = _mm512_cvtts_roundpd_epu64 (xd, 8);
  xi = _mm512_mask_cvtts_roundpd_epu64 (xi, m8, xd, 8);
  xi = _mm512_maskz_cvtts_roundpd_epu64 (m8, xd, 8);

  xi = _mm512_cvtts_ps_epi32 (x);
  xi = _mm512_mask_cvtts_ps_epi32 (xi, m16, x);
  xi = _mm512_maskz_cvtts_ps_epi32 (m16, x);
  xi = _mm512_cvtts_roundps_epi32 (x, 8);
  xi = _mm512_mask_cvtts_roundps_epi32 (xi, m16, x, 8);
  xi = _mm512_maskz_cvtts_roundps_epi32 (m16, x, 8);

  xi = _mm512_cvtts_ps_epi64 (hx);
  xi = _mm512_mask_cvtts_ps_epi64 (xi, m8, hx);
  xi = _mm512_maskz_cvtts_ps_epi64 (m8, hx);
  xi = _mm512_cvtts_roundps_epi64 (hx, 8);
  xi = _mm512_mask_cvtts_roundps_epi64 (xi, m8, hx, 8);
  xi = _mm512_maskz_cvtts_roundps_epi64 (m8, hx, 8);

  xi = _mm512_cvtts_ps_epu32 (x);
  xi = _mm512_mask_cvtts_ps_epu32 (xi, m16, x);
  xi = _mm512_maskz_cvtts_ps_epu32 (m16, x);
  xi = _mm512_cvtts_roundps_epu32 (x, 8);
  xi = _mm512_mask_cvtts_roundps_epu32 (xi, m16, x, 8);
  xi = _mm512_maskz_cvtts_roundps_epu32 (m16, x, 8);

  xi = _mm512_cvtts_ps_epu64 (hx);
  xi = _mm512_mask_cvtts_ps_epu64 (xi, m8, hx);
  xi = _mm512_maskz_cvtts_ps_epu64 (m8, hx);
  xi = _mm512_cvtts_roundps_epu64 (hx, 8);
  xi = _mm512_mask_cvtts_roundps_epu64 (xi, m8, hx, 8);
  xi = _mm512_maskz_cvtts_roundps_epu64 (m8, hx, 8);
}
