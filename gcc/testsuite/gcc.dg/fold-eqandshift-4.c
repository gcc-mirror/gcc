/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int sr30eq00(signed char x) { return ((x >> 4) & 0x30) == 0; }
int sr30ne00(signed char x) { return ((x >> 4) & 0x30) != 0; }
int sr30eq20(signed char z) { return ((z >> 4) & 0x30) == 0x20; }
int sr30ne20(signed char z) { return ((z >> 4) & 0x30) != 0x20; }
int sr30eq30(signed char x) { return ((x >> 4) & 0x30) == 0x30; }
int sr30ne30(signed char x) { return ((x >> 4) & 0x30) != 0x30; }
int sr33eq33(signed char x) { return ((x >> 4) & 0x33) == 0x33; }
int sr33ne33(signed char x) { return ((x >> 4) & 0x33) != 0x33; }

int ur30eq00(unsigned char z) { return ((z >> 4) & 0x30) == 0; }
int ur30ne00(unsigned char z) { return ((z >> 4) & 0x30) != 0; }
int ur30eq30(unsigned char z) { return ((z >> 4) & 0x30) == 0x30; }
int ur30ne30(unsigned char z) { return ((z >> 4) & 0x30) != 0x30; }
int ur33eq03(unsigned char x) { return ((x >> 4) & 0x33) == 0x03; }
int ur33ne03(unsigned char x) { return ((x >> 4) & 0x33) != 0x03; }
int ur33eq30(unsigned char z) { return ((z >> 4) & 0x33) == 0x30; }
int ur33ne30(unsigned char z) { return ((z >> 4) & 0x33) != 0x30; }
int ur33eq33(unsigned char z) { return ((z >> 4) & 0x33) == 0x33; }
int ur33ne33(unsigned char z) { return ((z >> 4) & 0x33) != 0x33; }

int sl30eq00(char x) { return ((char)(x << 4) & 0x30) == 0; }
int sl30ne00(char x) { return ((char)(x << 4) & 0x30) != 0; }
int sl30eq20(char x) { return ((char)(x << 4) & 0x30) == 0x20; }
int sl30ne20(char x) { return ((char)(x << 4) & 0x30) != 0x20; }
int sl30eq30(char x) { return ((char)(x << 4) & 0x30) == 0x30; }
int sl30ne30(char x) { return ((char)(x << 4) & 0x30) != 0x30; }
int sl33eq00(char x) { return ((char)(x << 4) & 0x33) == 0; }
int sl33ne00(char x) { return ((char)(x << 4) & 0x33) != 0; }
int sl33eq03(char z) { return ((char)(z << 4) & 0x33) == 0x03; }
int sl33ne03(char z) { return ((char)(z << 4) & 0x33) != 0x03; }
int sl33eq30(char x) { return ((char)(x << 4) & 0x33) == 0x30; }
int sl33ne30(char x) { return ((char)(x << 4) & 0x33) != 0x30; }
int sl33eq33(char z) { return ((char)(z << 4) & 0x33) == 0x33; }
int sl33ne33(char z) { return ((char)(z << 4) & 0x33) != 0x33; }

/* { dg-final { scan-tree-dump-not " >> " "optimized" } } */
/* { dg-final { scan-tree-dump-not " << " "optimized" } } */
/* { dg-final { scan-tree-dump-not "z_\[0-9\]\\(D\\)" "optimized" } } */
/* { dg-final { scan-tree-dump-times "return \[01\]" 14 "optimized" } } */
/* { dg-final { scan-tree-dump-times "char z\\)" 14 "optimized" } } */
/* { dg-final { scan-tree-dump-times "x_\[0-9\]\\(D\\)" 18 "optimized" } } */
/* { dg-final { scan-tree-dump-times "char x\\)" 18 "optimized" } } */

