/* { dg-do run { target { powerpc*-*-* && { p9vector_hw } } } } */
/* { dg-options "-mdejagnu-cpu=power9 -O2 " } */

#include <altivec.h> // vector

void abort (void);

int main() {
  int i;

  vector signed char vsca, vscr, vscexpt;
  vector unsigned char vuca, vucr, vucexpt;
  vector signed short int vssa, vssr, vssexpt;
  vector unsigned short int vusa, vusr, vusexpt;
  vector signed int vsia, vsir, vsiexpt;
  vector unsigned int vuia, vuir, vuiexpt;
  vector signed long long vslla, vsllr, vsllexpt;
  vector unsigned long long vulla, vullr, vullexpt;

  vsca = (vector signed char) {0, 1, 2, 3, 4, 5, 6, 7,
			       8, 9, 10, 11, 12, 13, 14, 15};

  vscexpt = (vector signed char) {8, 0, 1, 0, 2, 0, 1, 0,
				  3, 0, 1, 0, 2, 0, 1, 0};

  vuca = (vector unsigned char) {'0', '3', '6', '9', 'A', 'B', 'E', 'F',
				 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N'};
											
  vucexpt = (vector unsigned char) {4, 0, 1, 0, 0, 1, 0, 1,
				    0, 3, 0, 1, 0, 2, 0, 1};

  vssa = (vector short int) {0x1, 0x10, 0x100, 0x1000,
			     0x2, 0x20, 0x200, 0x2000};

  vssexpt = (vector short int) {0, 4, 8, 12, 1, 5, 9, 13};

  vusa = (vector unsigned short int) {0x4, 0x40, 0x400, 0x4000,
				      0x8, 0x80, 0x800, 0x8000};
  vusexpt = (vector unsigned short int) {2, 6, 10, 14, 3, 7, 11, 15};

  vsia = (vector int) {0x10000, 0x100000, 0x1000000, 0x10000000};
  vsiexpt = (vector int){16, 20, 24, 28};

  vuia = (vector unsigned int) {0x2, 0x20, 0x200, 0x2000};
  vuiexpt = (vector unsigned int){1, 5, 9, 13};

  vslla = (vector long long) {0x0000000000010000LL, 0x0001000100010000LL};
  vsllexpt = (vector long long){16, 16};

  vulla = (vector unsigned long long) {0x0000400000000000LL, 0x0080000000000000ULL};

  vullexpt = (vector unsigned long long) {46, 55};

  vscr = vec_cnttz (vsca);
  vucr = vec_cnttz (vuca);
  vssr = vec_cnttz (vssa);
  vusr = vec_cnttz (vusa);
  vsir = vec_cnttz (vsia);
  vuir = vec_cnttz (vuia);
  vsllr = vec_cnttz (vslla);
  vullr = vec_cnttz (vulla);

  for (i=0; i<16; i++) {
    if (vscr[i] != vscexpt[i])
      abort();

    if (vucr[i] != vucexpt[i])
      abort();
  }

  for (i=0; i<8; i++) {
    if (vssr[i] != vssexpt[i])
      abort();

    if (vusr[i] != vusexpt[i])
      abort();
  }

  for (i=0; i<4; i++) {
    if (vsir[i] != vsiexpt[i])
      abort();

    if (vuir[i] != vuiexpt[i])
      abort();
  }

  for (i=0; i<2; i++) {
    if (vsllr[i] != vsllexpt[i])
      abort();

    if (vullr[i] != vullexpt[i])
      abort();
  }	
}
