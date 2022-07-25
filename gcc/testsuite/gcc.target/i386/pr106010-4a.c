/* { dg-do compile } */
/* { dg-options "-O2 -mavx -ftree-vectorize -fvect-cost-model=unlimited -fdump-tree-slp-details" } */
/* { dg-final { scan-tree-dump-times "basic block part vectorized using (?:32|64) byte vectors" 6 "slp2" } }*/
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(4\) double>} 1 "slp2" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(8\) float>} 1 "slp2" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(4\) long long int>} 1 "slp2" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(8\) int>} 1 "slp2" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(16\) short int>} 1 "slp2" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(32\) char>} 1 "slp2" } } */

void
__attribute__((noipa))
foo_pd (_Complex double* a,
	_Complex double b1,
	_Complex double b2)
{
  a[0] = b1;
  a[1] = b2;
}

void
__attribute__((noipa))
foo_ps (_Complex float* a,
	_Complex float b1, _Complex float b2,
	_Complex float b3, _Complex float b4)
{
  a[0] = b1;
  a[1] = b2;
  a[2] = b3;
  a[3] = b4;
}

void
__attribute__((noipa))
foo_epi64 (_Complex long long* a,
	   _Complex long long b1,
	   _Complex long long b2)
{
  a[0] = b1;
  a[1] = b2;
}

void
__attribute__((noipa))
foo_epi32 (_Complex int* a,
	   _Complex int b1, _Complex int b2,
	   _Complex int b3, _Complex int b4)
{
  a[0] = b1;
  a[1] = b2;
  a[2] = b3;
  a[3] = b4;
}

void
__attribute__((noipa))
foo_epi16 (_Complex short* a,
	   _Complex short b1, _Complex short b2,
	   _Complex short b3, _Complex short b4,
	   _Complex short b5, _Complex short b6,
	   _Complex short b7,_Complex short b8)
{
  a[0] = b1;
  a[1] = b2;
  a[2] = b3;
  a[3] = b4;
  a[4] = b5;
  a[5] = b6;
  a[6] = b7;
  a[7] = b8;
}

void
__attribute__((noipa))
foo_epi8 (_Complex char* a,
	  _Complex char b1, _Complex char b2,
	  _Complex char b3, _Complex char b4,
	  _Complex char b5, _Complex char b6,
	  _Complex char b7,_Complex char b8,
	  _Complex char b9, _Complex char b10,
	  _Complex char b11, _Complex char b12,
	  _Complex char b13, _Complex char b14,
	  _Complex char b15,_Complex char b16)
{
  a[0] = b1;
  a[1] = b2;
  a[2] = b3;
  a[3] = b4;
  a[4] = b5;
  a[5] = b6;
  a[6] = b7;
  a[7] = b8;
  a[8] = b9;
  a[9] = b10;
  a[10] = b11;
  a[11] = b12;
  a[12] = b13;
  a[13] = b14;
  a[14] = b15;
  a[15] = b16;
}
