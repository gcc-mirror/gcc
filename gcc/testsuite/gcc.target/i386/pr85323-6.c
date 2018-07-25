/* PR target/85323 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512bw -fdump-tree-optimized" } */

#include <x86intrin.h>

struct S1 { __m128i a, b, c, d, e, f, g, h, i; } s1;
struct S2 { __m256i a, b, c, d, e, f, g, h, i; } s2;
struct S3 { __m512i a, b, c, d, e, f, g, h, i; } s3;

/* { dg-final { scan-tree-dump-times "s1.a = \{ -4342213319840130048, -761680639942076944 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.b = \{ 16777216, 149499221639168 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.c = \{ 2623346860254860648, -763360136839241728 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.d = \{ 35871495301330685, 2005711373062887255 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.e = \{ 128, 1729384589077512192 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.f = \{ 655836773112359254, 2005509209063424011 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.g = \{ -157301717633283, -300131636150806697 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.h = \{ -128, -576458420136181760 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.i = \{ 655836777273157974, -300052325173559301 \};" 1 "optimized" } } */

void
foo (void)
{
  __m128i a = _mm_set_epi64x (0xdeadbeefcafebabeULL, 0xfee1deadfeedfaceULL);
  __m128i b = _mm_set_epi64x (3, 9);
  __m128i c = _mm_set_epi32 (0xc00010ffU, 0x12345678U, 0xfedcba98U, 0x80000001U);
  __m128i d = _mm_set_epi32 (3, 32, -6, 24);
  __m128i e = _mm_set_epi16 (0xdead, 0xbeef, 0xcafe, 0xbabe,
			     0x1234, 0x0012, 0x8001, 0xa55a);
  __m128i f = _mm_set_epi16 (3, 16, -1, 12, 1, 0, 5, 2);
  s1.a = _mm_sllv_epi64 (a, b);
  s1.b = _mm_sllv_epi32 (c, d);
  s1.c = _mm_sllv_epi16 (e, f);
  s1.d = _mm_srlv_epi64 (a, b);
  s1.e = _mm_srlv_epi32 (c, d);
  s1.f = _mm_srlv_epi16 (e, f);
  s1.g = _mm_srav_epi64 (a, b);
  s1.h = _mm_srav_epi32 (c, d);
  s1.i = _mm_srav_epi16 (e, f);
}

/* { dg-final { scan-tree-dump-times "s2.a = \{ 6722813395751927808, 0, 0, -1523361279884153888 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.b = \{ 9177596069264525312, 1851607040, -81985531201716224, 76543602090093808 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.c = \{ 1008895103428722688, -5985166321598332416, 2623346860254860648, -763360136839241728 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.d = \{ 2189249818860, 0, 0, 1002855686531443627 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.e = \{ 114276044520956448, 130489, -81985531201716224, 3377704168205116 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.f = \{ 289076540546023424, 3115407575762206978, 655836773112359254, 2005509209063424011 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.g = \{ 2189249818860, 0, -1, -150065818075403349 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.h = \{ -29839143554899424, -4294836807, -81985526906748929, -1125895459165380 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.i = \{ -287384211757400064, 3115618685752836354, 655836777273157974, -300052325173559301 \};" 1 "optimized" } } */

void
bar (void)
{
  __m256i a = _mm256_set_epi64x (0xdeadbeefcafebabeULL, 0xfee1deadfeedfaceULL,
				 0x123456789abcdef0ULL, 0x0fedcba987654321ULL);
  __m256i b = _mm256_set_epi64x (4, 65, -2, 19);
  __m256i c = _mm256_set_epi32 (0xc00010ffU, 0x12345678U, 0xfedcba98U, 0x80000001U,
				0xdeadbeefU, 0x0fedcba9U, 0xcafebabeU, 0x00111100U);
  __m256i d = _mm256_set_epi32 (12, 1, 0, -2, 32, 11, 7, 3);
  __m256i e = _mm256_set_epi16 (0xdead, 0xbeef, 0xcafe, 0xbabe,
				0x1234, 0x0012, 0x8001, 0xa55a,
				0x5678, 0x9abc, 0xdef0, 0x1020,
				0x8070, 0x6543, 0x129f, 0);
  __m256i f = _mm256_set_epi16 (3, 16, -1, 12, 1, 0, 5, 2, 1, 2, 3, 4, 5, 6, 7, 8);
  s2.a = _mm256_sllv_epi64 (a, b);
  s2.b = _mm256_sllv_epi32 (c, d);
  s2.c = _mm256_sllv_epi16 (e, f);
  s2.d = _mm256_srlv_epi64 (a, b);
  s2.e = _mm256_srlv_epi32 (c, d);
  s2.f = _mm256_srlv_epi16 (e, f);
  s2.g = _mm256_srav_epi64 (a, b);
  s2.h = _mm256_srav_epi32 (c, d);
  s2.i = _mm256_srav_epi16 (e, f);
}

/* { dg-final { scan-tree-dump-times "s3.a = \{ 6592671264835730432, 5247073869855161280, 1147797409030816545, -161076958856481380, 6722813395751927808, 0, 0, -1523361279884153888 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.b = \{ -4611667331015735296, 6592669523180452796, 2541551364173987968, 1068969636, 9177596069264525312, 1851607040, -81985531201716224, 76543602090093808 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.c = \{ 6233191819462621886, 8070591269736295416, 8610979175836155904, 40534596407293308, 1008895103428722688, -5985166321598332416, 2623346860254860648, -763360136839241728 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.d = \{ 31339240204107613, 327942116865947580, 1147797409030816545, 9183102797140655463, 2189249818860, 0, 0, 1002855686531443627 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.e = \{ -4611667331024543200, 31339239126560699, 81985526923526144, 66810602, 114276044520956448, 130489, -81985531201716224, 3377704168205116 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.f = \{ 125466298768407230, 36028797018976959, 107269861939347456, 563225682730335, 289076540546023424, 3115407575762206978, 655836773112359254, 2005509209063424011 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.g = \{ -4689556814856355, 327942116865947580, 1147797409030816545, -40269239714120345, 2189249818860, 0, -1, -150065818075403349 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.h = \{ -4611667331024543200, -4689554671177797, 81985531184939008, 66810602, -29839143554899424, -4294836807, -81985526906748929, -1125895459165380 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.i = \{ -18648885549352258, -36028797018901825, -36599031236919297, 844154124885343, -287384211757400064, 3115618685752836354, 655836777273157974, -300052325173559301 \};" 1 "optimized" } } */

void
baz (void)
{
  __m512i a = _mm512_set_epi64 (0xdeadbeefcafebabeULL, 0xfee1deadfeedfaceULL,
				0x123456789abcdef0ULL, 0x0fedcba987654321ULL,
				0xfee1deadfeedfaceULL, 0x0fedcba987654321ULL,
				0x123456789abcdef0ULL, 0xdeadbeefcafebabeULL);
  __m512i b = _mm512_set_epi64 (4, 65, -2, 19, 1, 0, 2, 9);
  __m512i c = _mm512_set_epi32 (0xc00010ffU, 0x12345678U, 0xfedcba98U, 0x80000001U,
				0xdeadbeefU, 0x0fedcba9U, 0xcafebabeU, 0x00111100U,
				0, 0x0fedcba9U, 0x12345678U, 0x80000001U,
				0xdeadbeefU, 0xdeadbeefU, 0xc00010ffU, 0x00111100U);
  __m512i d = _mm512_set_epi32 (12, 1, 0, -2, 32, 11, 7, 3, 1, 2, 4, 7, 9, 2, 0, 3);
  __m512i e = _mm512_set_epi16 (0xdead, 0xbeef, 0xcafe, 0xbabe,
				0x1234, 0x0012, 0x8001, 0xa55a,
				0x5678, 0x9abc, 0xdef0, 0x1020,
				0x8070, 0x6543, 0x129f, 0,
				0x0012, 0x8001, 0xcafe, 0xbabe,
				0xbeef, 0xcafe, 0x9abc, 0xdef0,
				0x8070, 0x6543, 0x129f, 0xcafe,
				0xdead, 0xbeef, 0xcafe, 0xbabe);
  __m512i f = _mm512_set_epi16 (3, 16, -1, 12, 1, 0, 5, 2, 1, 2, 3, 4, 5, 6, 7, 8,
				3, 9, 2, 1, 7, 3, -12, 26, 8, 15, 17, 2, 7, 0, 3, 0);
  s3.a = _mm512_sllv_epi64 (a, b);
  s3.b = _mm512_sllv_epi32 (c, d);
  s3.c = _mm512_sllv_epi16 (e, f);
  s3.d = _mm512_srlv_epi64 (a, b);
  s3.e = _mm512_srlv_epi32 (c, d);
  s3.f = _mm512_srlv_epi16 (e, f);
  s3.g = _mm512_srav_epi64 (a, b);
  s3.h = _mm512_srav_epi32 (c, d);
  s3.i = _mm512_srav_epi16 (e, f);
}
