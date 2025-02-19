/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-256" } */
/* { dg-final { scan-assembler-times "vcvtuw2ph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtuw2ph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtuw2ph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtw2ph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtw2ph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vcvtw2ph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivpd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivpd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivpd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vdivps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfcmaddcph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfcmaddcph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vfcmaddcph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfcmulcph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vfcmulcph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vfcmulcph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vfixupimmpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1  }  } */
/* { dg-final { scan-assembler-times "vfixupimmpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfixupimmpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfixupimmps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1  }  } */
/* { dg-final { scan-assembler-times "vfixupimmps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfixupimmps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...pd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...pd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd231pd\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...pd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...ph\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...ph\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd231ph\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...ph\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...ps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...ps\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd231ps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmadd...ps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddcph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddcph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vfmaddcph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...pd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...pd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub231pd\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...pd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...ph\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...ph\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub231ph\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...ph\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...ps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...ps\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub231ps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmaddsub...ps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsub...pd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsub...pd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsub231pd\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsub...pd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsub...ph\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsub...ph\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsub231ph\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsub...ph\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsub...ps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsub...ps\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsub231ps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsub...ps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsubadd...pd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsubadd...pd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsubadd231pd\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsubadd...pd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsubadd...ph\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsubadd...ph\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsubadd231ph\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsubadd...ph\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsubadd...ps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsubadd...ps\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsubadd231ps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmsubadd...ps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfmulcph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vfmulcph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vfmulcph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vfnmadd...pd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmadd...pd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmadd231pd\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmadd...pd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmadd...ph\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmadd...ph\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmadd231ph\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmadd...ph\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmadd...ps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmadd...ps\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmadd231ps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmadd...ps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmsub...ps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmsub...ps\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmsub231ps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmsub...ps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmsub...pd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmsub...pd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmsub231pd\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmsub...pd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmsub...ph\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmsub...ph\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmsub231ph\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vfnmsub...ph\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vgetexppd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vgetexppd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vgetexppd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vgetexpph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vgetexpph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vgetexpph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vgetexpps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vgetexpps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vgetexpps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vgetmantpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vgetmantpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vgetmantpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vgetmantph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vgetmantph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vgetmantph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vgetmantps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vgetmantps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vgetmantps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 }  } */
/* { dg-final { scan-assembler-times "vmaxpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1  }  } */
/* { dg-final { scan-assembler-times "vmaxpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vmaxpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vmaxph\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vmaxph\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vmaxph\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vmaxps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1  }  } */
/* { dg-final { scan-assembler-times "vmaxps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vmaxps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vminpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1  }  } */
/* { dg-final { scan-assembler-times "vminpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vminpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vminph\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vminph\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2  }  } */
/* { dg-final { scan-assembler-times "vminph\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vminps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1  }  } */
/* { dg-final { scan-assembler-times "vminps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vminps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vmulpd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vmulpd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vmulpd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vmulph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vmulph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vmulph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vmulps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vmulps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vmulps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vrangepd\[ \\t\]+\[^\$\n\]*\\$\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1  }  } */
/* { dg-final { scan-assembler-times "vrangepd\[ \\t\]+\[^\$\n\]*\\$\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vrangepd\[ \\t\]+\[^\$\n\]*\\$\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vrangeps\[ \\t\]+\[^\$\n\]*\\$\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1  }  } */
/* { dg-final { scan-assembler-times "vrangeps\[ \\t\]+\[^\$\n\]*\\$\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vrangeps\[ \\t\]+\[^\$\n\]*\\$\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vreducepd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1  }  } */
/* { dg-final { scan-assembler-times "vreducepd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vreducepd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vreduceph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1  }  } */
/* { dg-final { scan-assembler-times "vreduceph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vreduceph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vreduceps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1  }  } */
/* { dg-final { scan-assembler-times "vreduceps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vreduceps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vrndscalepd\[ \\t\]+\\S*,\[ \\t\]+\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vrndscalepd\[ \\t\]+\\S*,\[ \\t\]+\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vrndscalepd\[ \\t\]+\\S*,\[ \\t\]+\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vrndscaleph\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1  }  } */
/* { dg-final { scan-assembler-times "vrndscaleph\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vrndscaleph\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vrndscaleps\[ \\t\]+\\S*,\[ \\t\]+\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vrndscaleps\[ \\t\]+\\S*,\[ \\t\]+\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vrndscaleps\[ \\t\]+\\S*,\[ \\t\]+\{sae\}\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vscalefpd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vscalefpd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vscalefpd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vscalefph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vscalefph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vscalefph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vscalefps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vscalefps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vscalefps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsqrtpd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsqrtpd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsqrtpd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsqrtph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsqrtph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsqrtph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsqrtps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsqrtps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsqrtps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsubpd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsubpd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsubpd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsubph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsubph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsubph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsubps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsubps\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1  }  } */
/* { dg-final { scan-assembler-times "vsubps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1  }  } */

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
  xh = _mm256_cvt_roundepu16_ph (xi, 4);
  xh = _mm256_mask_cvt_roundepu16_ph (xh, m16, xi, 8);
  xh = _mm256_maskz_cvt_roundepu16_ph (m16, xi, 11);

  xh = _mm256_cvt_roundepi16_ph (xi, 4);
  xh = _mm256_mask_cvt_roundepi16_ph (xh, m16, xi, 8);
  xh = _mm256_maskz_cvt_roundepi16_ph (m16, xi, 11);
}

void extern
avx10_2_test_2 (void)
{
  xd = _mm256_div_round_pd (xd, xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_div_round_pd (xd, m8, xd, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_div_round_pd (m8, xd, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xh = _mm256_div_round_ph (xh, xh, 8);
  xh = _mm256_mask_div_round_ph (xh, m16, xh, xh, 8);
  xh = _mm256_maskz_div_round_ph (m16, xh, xh, 11);

  x = _mm256_div_round_ps (x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_div_round_ps (x, m16, x, x, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_div_round_ps (m16, x, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_3 (void)
{
  xh = _mm256_fcmadd_round_pch (xh, xh, xh, 8);
  xh = _mm256_mask_fcmadd_round_pch (xh, m8, xh, xh, 8);
  xh = _mm256_mask3_fcmadd_round_pch (xh, xh, xh, m8, 8);
  xh = _mm256_maskz_fcmadd_round_pch (m8, xh, xh, xh, 11);
}

void extern
avx10_2_test_4 (void)
{
  xh = _mm256_fcmul_round_pch (xh, xh, 8);
  xh = _mm256_mask_fcmul_round_pch (xh, m8, xh, xh, 8);
  xh = _mm256_maskz_fcmul_round_pch (m8, xh, xh, 11);
}

void extern
avx10_2_test_5 (void)
{
  xh = _mm256_cmul_round_pch (xh, xh, 8);
  xh = _mm256_mask_cmul_round_pch (xh, m8, xh, xh, 8);
  xh = _mm256_maskz_cmul_round_pch (m8, xh, xh, 11);
}

void extern
avx10_2_test_6 (void)
{
  xd = _mm256_fixupimm_round_pd (xd, xd, xi, 3, _MM_FROUND_NO_EXC);
  xd = _mm256_mask_fixupimm_round_pd (xd, m8, xd, xi, 3, _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_fixupimm_round_pd (m8, xd, xd, xi, 3, _MM_FROUND_NO_EXC);

  x = _mm256_fixupimm_round_ps (x, x, xi, 3, _MM_FROUND_NO_EXC);
  x = _mm256_mask_fixupimm_round_ps (x, m8, x, xi, 3, _MM_FROUND_NO_EXC);
  x = _mm256_maskz_fixupimm_round_ps (m8, x, x, xi, 3, _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_7 (void)
{
  xd = _mm256_fmadd_round_pd (xd, xd, xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_fmadd_round_pd (xd, m8, xd, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_mask3_fmadd_round_pd (xd, xd, xd, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_fmadd_round_pd (m8, xd, xd, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xh = _mm256_fmadd_round_ph (xh, xh, xh, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xh = _mm256_mask_fmadd_round_ph (xh, m16, xh, xh, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xh = _mm256_mask3_fmadd_round_ph (xh, xh, xh, m16, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xh = _mm256_maskz_fmadd_round_ph (m16, xh, xh, xh, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  x = _mm256_fmadd_round_ps (x, x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_fmadd_round_ps (x, m8, x, x, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  x = _mm256_mask3_fmadd_round_ps (x, x, x, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_fmadd_round_ps (m8, x, x, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_8 (void)
{
  xh = _mm256_fmadd_round_pch (xh, xh, xh, 8);
  xh = _mm256_mask_fmadd_round_pch (xh, m8, xh, xh, 8);
  xh = _mm256_mask3_fmadd_round_pch (xh, xh, xh, m8, 8);
  xh = _mm256_maskz_fmadd_round_pch (m8, xh, xh, xh, 11);
}

void extern
avx10_2_test_9 (void)
{
  xd = _mm256_fmaddsub_round_pd (xd, xd, xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_fmaddsub_round_pd (xd, m8, xd, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_mask3_fmaddsub_round_pd (xd, xd, xd, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_fmaddsub_round_pd (m8, xd, xd, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xh = _mm256_fmaddsub_round_ph (xh, xh, xh, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xh = _mm256_mask_fmaddsub_round_ph (xh, m8, xh, xh, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xh = _mm256_mask3_fmaddsub_round_ph (xh, xh, xh, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xh = _mm256_maskz_fmaddsub_round_ph (m8, xh, xh, xh, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  x = _mm256_fmaddsub_round_ps (x, x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_fmaddsub_round_ps (x, m8, x, x, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  x = _mm256_mask3_fmaddsub_round_ps (x, x, x, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_fmaddsub_round_ps (m8, x, x, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_10 (void)
{
  xd = _mm256_fmsub_round_pd (xd, xd, xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_fmsub_round_pd (xd, m8, xd, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_mask3_fmsub_round_pd (xd, xd, xd, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_fmsub_round_pd (m8, xd, xd, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xh = _mm256_fmsub_round_ph (xh, xh, xh, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xh = _mm256_mask_fmsub_round_ph (xh, m8, xh, xh, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xh = _mm256_mask3_fmsub_round_ph (xh, xh, xh, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xh = _mm256_maskz_fmsub_round_ph (m8, xh, xh, xh, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  x = _mm256_fmsub_round_ps (x, x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_fmsub_round_ps (x, m8, x, x, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  x = _mm256_mask3_fmsub_round_ps (x, x, x, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_fmsub_round_ps (m8, x, x, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_11 (void)
{
  xd = _mm256_fmsubadd_round_pd (xd, xd, xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_fmsubadd_round_pd (xd, m8, xd, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_mask3_fmsubadd_round_pd (xd, xd, xd, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_fmsubadd_round_pd (m8, xd, xd, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xh = _mm256_fmsubadd_round_ph (xh, xh, xh, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xh = _mm256_mask_fmsubadd_round_ph (xh, m8, xh, xh, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xh = _mm256_mask3_fmsubadd_round_ph (xh, xh, xh, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xh = _mm256_maskz_fmsubadd_round_ph (m8, xh, xh, xh, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  x = _mm256_fmsubadd_round_ps (x, x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_fmsubadd_round_ps (x, m8, x, x, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  x = _mm256_mask3_fmsubadd_round_ps (x, x, x, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_fmsubadd_round_ps (m8, x, x, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_12 (void)
{
  xh = _mm256_fmul_round_pch (xh, xh, 8);
  xh = _mm256_mask_fmul_round_pch (xh, m8, xh, xh, 8);
  xh = _mm256_maskz_fmul_round_pch (m8, xh, xh, 11);
}

void extern
avx10_2_test_13 (void)
{
  xh = _mm256_mul_round_pch (xh, xh, 8);
  xh = _mm256_mask_mul_round_pch (xh, m8, xh, xh, 8);
  xh = _mm256_maskz_mul_round_pch (m8, xh, xh, 11);
}

void extern
avx10_2_test_14 (void)
{
  xd = _mm256_fnmadd_round_pd (xd, xd, xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_fnmadd_round_pd (xd, m8, xd, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_mask3_fnmadd_round_pd (xd, xd, xd, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_fnmadd_round_pd (m8, xd, xd, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xh = _mm256_fnmadd_round_ph (xh, xh, xh, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xh = _mm256_mask_fnmadd_round_ph (xh, m8, xh, xh, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xh = _mm256_mask3_fnmadd_round_ph (xh, xh, xh, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xh = _mm256_maskz_fnmadd_round_ph (m8, xh, xh, xh, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  x = _mm256_fnmadd_round_ps (x, x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_fnmadd_round_ps (x, m8, x, x, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  x = _mm256_mask3_fnmadd_round_ps (x, x, x, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_fnmadd_round_ps (m8, x, x, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_15 (void)
{
  xd = _mm256_fnmsub_round_pd (xd, xd, xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_fnmsub_round_pd (xd, m8, xd, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_mask3_fnmsub_round_pd (xd, xd, xd, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_fnmsub_round_pd (m8, xd, xd, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xh = _mm256_fnmsub_round_ph (xh, xh, xh, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xh = _mm256_mask_fnmsub_round_ph (xh, m8, xh, xh, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xh = _mm256_mask3_fnmsub_round_ph (xh, xh, xh, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  xh = _mm256_maskz_fnmsub_round_ph (m8, xh, xh, xh, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  x = _mm256_fnmsub_round_ps (x, x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_fnmsub_round_ps (x, m8, x, x, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  x = _mm256_mask3_fnmsub_round_ps (x, x, x, m8, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_fnmsub_round_ps (m8, x, x, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_16 (void)
{
  xd = _mm256_getexp_round_pd (xd, _MM_FROUND_NO_EXC);
  xd = _mm256_mask_getexp_round_pd (xd, m8, xd, _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_getexp_round_pd (m8, xd, _MM_FROUND_NO_EXC);

  xh = _mm256_getexp_round_ph (xh, _MM_FROUND_NO_EXC);
  xh = _mm256_mask_getexp_round_ph (xh, m16, xh, _MM_FROUND_NO_EXC);
  xh = _mm256_maskz_getexp_round_ph (m16, xh, _MM_FROUND_NO_EXC);

  x = _mm256_getexp_round_ps (x, _MM_FROUND_NO_EXC);
  x = _mm256_mask_getexp_round_ps (x, m8, x, _MM_FROUND_NO_EXC);
  x = _mm256_maskz_getexp_round_ps (m8, x, _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_17 (void)
{
  xd = _mm256_getmant_round_pd (xd, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_src,
			        _MM_FROUND_NO_EXC);
  xd = _mm256_mask_getmant_round_pd (xd, m8, xd, _MM_MANT_NORM_p75_1p5,
				     _MM_MANT_SIGN_src, _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_getmant_round_pd (m8, xd, _MM_MANT_NORM_p75_1p5,
				      _MM_MANT_SIGN_src, _MM_FROUND_NO_EXC);

  xh = _mm256_getmant_round_ph (xh, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_src,
			        _MM_FROUND_NO_EXC);
  xh = _mm256_mask_getmant_round_ph (xh, m16, xh, _MM_MANT_NORM_p75_1p5,
				     _MM_MANT_SIGN_src, _MM_FROUND_NO_EXC);
  xh = _mm256_maskz_getmant_round_ph (m16, xh, _MM_MANT_NORM_p75_1p5,
				      _MM_MANT_SIGN_src, _MM_FROUND_NO_EXC);

  x = _mm256_getmant_round_ps (x, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_src,
			       _MM_FROUND_NO_EXC);
  x = _mm256_mask_getmant_round_ps (x, m8, x, _MM_MANT_NORM_p75_1p5,
				    _MM_MANT_SIGN_src, _MM_FROUND_NO_EXC);
  x = _mm256_maskz_getmant_round_ps (m8, x, _MM_MANT_NORM_p75_1p5,
				     _MM_MANT_SIGN_src, _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_18 (void)
{
  xd = _mm256_max_round_pd (xd, xd, _MM_FROUND_NO_EXC);
  xd = _mm256_mask_max_round_pd (xd, m8, xd, xd, _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_max_round_pd (m8, xd, xd, _MM_FROUND_NO_EXC);

  xh = _mm256_max_round_ph (xh, xh, _MM_FROUND_NO_EXC);
  xh = _mm256_mask_max_round_ph (xh, m16, xh, xh, _MM_FROUND_NO_EXC);
  xh = _mm256_maskz_max_round_ph (m16, xh, xh, _MM_FROUND_NO_EXC);

  x = _mm256_max_round_ps (x, x, _MM_FROUND_NO_EXC);
  x = _mm256_mask_max_round_ps (x, m8, x, x, _MM_FROUND_NO_EXC);
  x = _mm256_maskz_max_round_ps (m8, x, x, _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_19 (void)
{
  xd = _mm256_min_round_pd (xd, xd, _MM_FROUND_NO_EXC);
  xd = _mm256_mask_min_round_pd (xd, m8, xd, xd, _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_min_round_pd (m8, xd, xd, _MM_FROUND_NO_EXC);

  xh = _mm256_min_round_ph (xh, xh, _MM_FROUND_NO_EXC);
  xh = _mm256_mask_min_round_ph (xh, m16, xh, xh, _MM_FROUND_NO_EXC);
  xh = _mm256_maskz_min_round_ph (m16, xh, xh, _MM_FROUND_NO_EXC);

  x = _mm256_min_round_ps (x, x, _MM_FROUND_NO_EXC);
  x = _mm256_mask_min_round_ps (x, m8, x, x, _MM_FROUND_NO_EXC);
  x = _mm256_maskz_min_round_ps (m8, x, x, _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_20 (void)
{
  xd = _mm256_mul_round_pd (xd, xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_mul_round_pd (xd, m8, xd, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_mul_round_pd (m8, xd, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xh = _mm256_mul_round_ph (xh, xh, 8);
  xh = _mm256_mask_mul_round_ph (xh, m16, xh, xh, 8);
  xh = _mm256_maskz_mul_round_ph (m16, xh, xh, 11);

  x = _mm256_mul_round_ps (x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_mul_round_ps (x, m8, x, x, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_mul_round_ps (m8, x, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_21 (void)
{
  xd = _mm256_range_round_pd (xd, xd, 15, _MM_FROUND_NO_EXC);
  xd = _mm256_mask_range_round_pd (xd, m8, xd, xd, 15, _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_range_round_pd (m8, xd, xd, 15, _MM_FROUND_NO_EXC);

  x = _mm256_range_round_ps (x, x, 15, _MM_FROUND_NO_EXC);
  x = _mm256_mask_range_round_ps (x, m16, x, x, 15, _MM_FROUND_NO_EXC);
  x = _mm256_maskz_range_round_ps (m16, x, x, 15, _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_22 (void)
{
  xd = _mm256_reduce_round_pd (xd, 123, _MM_FROUND_NO_EXC);
  xd = _mm256_mask_reduce_round_pd (xd, m8, xd, 123, _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_reduce_round_pd (m8, xd, 123, _MM_FROUND_NO_EXC);

  xh = _mm256_reduce_round_ph (xh, 123, _MM_FROUND_NO_EXC);
  xh = _mm256_mask_reduce_round_ph (xh, m16, xh, 123, _MM_FROUND_NO_EXC);
  xh = _mm256_maskz_reduce_round_ph (m16, xh, 123, _MM_FROUND_NO_EXC);

  x = _mm256_reduce_round_ps (x, 123, _MM_FROUND_NO_EXC);
  x = _mm256_mask_reduce_round_ps (x, m8, x, 123, _MM_FROUND_NO_EXC);
  x = _mm256_maskz_reduce_round_ps (m8, x, 123, _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_23 (void)
{
  xd = _mm256_roundscale_round_pd (xd, 0x42, _MM_FROUND_NO_EXC);
  xd = _mm256_mask_roundscale_round_pd (xd, 2, xd, 0x42, _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_roundscale_round_pd (2, xd, 0x42, _MM_FROUND_NO_EXC);

  xh = _mm256_roundscale_round_ph (xh, 123, 8);
  xh = _mm256_mask_roundscale_round_ph (xh, m16, xh, 123, 8);
  xh = _mm256_maskz_roundscale_round_ph (m16, xh, 123, 8);

  x = _mm256_roundscale_round_ps (x, 0x42, _MM_FROUND_NO_EXC);
  x = _mm256_mask_roundscale_round_ps (x, 2, x, 0x42, _MM_FROUND_NO_EXC);
  x = _mm256_maskz_roundscale_round_ps (2, x, 0x42, _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_24 (void)
{
  xd = _mm256_scalef_round_pd (xd, xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_scalef_round_pd (xd, m8, xd, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_scalef_round_pd (m8, xd, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xh = _mm256_scalef_round_ph (xh, xh, 8);
  xh = _mm256_mask_scalef_round_ph (xh, m16, xh, xh, 8);
  xh = _mm256_maskz_scalef_round_ph (m16, xh, xh, 11);

  x = _mm256_scalef_round_ps (x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_scalef_round_ps (x, m8, x, x, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_scalef_round_ps (m8, x, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_25 (void)
{
  xd = _mm256_sqrt_round_pd (xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_sqrt_round_pd (xd, m8, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_sqrt_round_pd (m8, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);

  xh = _mm256_sqrt_round_ph (xh, 4);
  xh = _mm256_mask_sqrt_round_ph (xh, m16, xh, 8);
  xh = _mm256_maskz_sqrt_round_ph (m16, xh, 11);

  x = _mm256_sqrt_round_ps (x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_sqrt_round_ps (x, m8, x, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_sqrt_round_ps (m8, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}

void extern
avx10_2_test_26 (void)
{
  xd = _mm256_sub_round_pd (xd, xd, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  xd = _mm256_mask_sub_round_pd (xd, m8, xd, xd, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  xd = _mm256_maskz_sub_round_pd (m8, xd, xd, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
  
  xh = _mm256_sub_round_ph (xh, xh, 8);
  xh = _mm256_mask_sub_round_ph (xh, m16, xh, xh, 8);
  xh = _mm256_maskz_sub_round_ph (m16, xh, xh, 11);

  x = _mm256_sub_round_ps (x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm256_mask_sub_round_ps (x, m8, x, x, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x = _mm256_maskz_sub_round_ps (m8, x, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}
