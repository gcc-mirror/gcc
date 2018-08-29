/* PR target/85323 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512bw -fdump-tree-optimized" } */

#include <x86intrin.h>

struct S1 { __m128i a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r; } s1;
struct S2 { __m256i a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r; } s2;
struct S3 { __m512i a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r; } s3;

/* { dg-final { scan-tree-dump-times "s1.a = \{ 0, -761680639942076944 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.b = \{ 8, 2443359168 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.c = \{ -7953356941935760688, 1475401200 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.d = \{ 2295775699285163865, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.e = \{ 2295594817813479424, 1729384589077512192 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.f = \{ 163818436714037248, 2005535447018635264 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.g = \{ 0, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.h = \{ -10248191400214528, -576458420098004273 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.i = \{ 163818436445601792, 63319 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.j = \{ -1288615670851851040, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.k = \{ -2623536998454919168, 597996886556672 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.l = \{ -8286618366559387648, 206436520996736 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.m = \{ 0, 125356960816430453 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.n = \{ 71737335514923008, 1193046 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.o = \{ 2533274794590208, 31243722414882909 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.p = \{ -78650858816642, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.q = \{ 4293918720, 149130 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.r = \{ 65535, 4294967295 \};" 1 "optimized" } } */

void
foo (void)
{
  __m128i a = _mm_set_epi64x (0xdeadbeefcafebabeULL, 0xfee1deadfeedfaceULL);
  __m128i b = _mm_set1_epi64x (3);
  __m128i c = _mm_set_epi32 (0xc00010ffU, 0x12345678U, 0xfedcba98U, 0x80000001U);
  __m128i d = _mm_set_epi16 (0xdead, 0xbeef, 0xcafe, 0xbabe,
			     0x1234, 0x0012, 0x8001, 0xa55a);
  s1.a = _mm_maskz_sll_epi64 (0x2, a, b);
  s1.b = _mm_maskz_sll_epi32 (0x5, c, b);
  s1.c = _mm_maskz_sll_epi16 (0x3b, d, b);
  s1.d = _mm_maskz_srl_epi64 (0x1, a, b);
  s1.e = _mm_maskz_srl_epi32 (0xa, c, b);
  s1.f = _mm_maskz_srl_epi16 (0xca, d, b);
  s1.g = _mm_maskz_sra_epi64 (0, a, b);
  s1.h = _mm_maskz_sra_epi32 (0xe, c, b);
  s1.i = _mm_maskz_sra_epi16 (0x18, d, b);
  s1.j = _mm_maskz_slli_epi64 (0x1, a, 4);
  s1.k = _mm_maskz_slli_epi32 (0xa, c, 5);
  s1.l = _mm_maskz_slli_epi16 (0x7c, d, 6);
  s1.m = _mm_maskz_srli_epi64 (0x2, a, 7);
  s1.n = _mm_maskz_srli_epi32 (0x7, c, 8);
  s1.o = _mm_maskz_srli_epi16 (0x9a, d, 9);
  s1.p = _mm_maskz_srai_epi64 (0x1, a, 10);
  s1.q = _mm_maskz_srai_epi32 (0x5, c, 11);
  s1.r = _mm_maskz_srai_epi16 (0x39, d, 17);
}

/* { dg-final { scan-tree-dump-times "s2.a = \{ 0, 0, -1288615670851851040, -1523361279884153888 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.b = \{ -5770329518966239232, 4275878544, -1311768499227459584, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.c = \{ 504495724104253440, 7457960982925541888, 1070496, 44000 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.d = \{ 71737338064426034, 81985529216486895, 0, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.e = \{ 914208369051435008, 1002855682292056064, 134217728, 864692292391272448 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.f = \{ 0, 389279893024604160, 81909218222803541, 13117042920363 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.g = \{ 0, 81985529216486895, 0, -150065818075403349 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.h = \{ 69904, -150065822314790912, 4160749568, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.i = \{ -574490427446984704, 389554405625561088, 81909222383550464, 4239391659 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.j = \{ 9182379272246532360, -7952596333999229056, -644307835425925520, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.k = \{ 6906085035741282304, -3046722668750438400, -2623536998454919136, 1183502080 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.l = \{ 2017701418625925120, 2048, -8286623314357496192, -6106881094714347648 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.m = \{ 0, 10248191152060861, 0, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.n = \{ 4369, 62678476385157120, 71737335514923008, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.o = \{ 0, 12103754718314496, 2533274794590290, 31243722421501952 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.p = \{ 1120895907256656, 0, 0, -2344778407428178 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.q = \{ -1864947814366686, 130489, 4293918720, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.r = \{ -281474976710656, 4294901760, 65535, 65535 \};" 1 "optimized" } } */

void
bar (void)
{
  __m256i a = _mm256_set_epi64x (0xdeadbeefcafebabeULL, 0xfee1deadfeedfaceULL,
				 0x123456789abcdef0ULL, 0x0fedcba987654321ULL);
  __m128i b = _mm_set1_epi64x (4);
  __m256i c = _mm256_set_epi32 (0xc00010ffU, 0x12345678U, 0xfedcba98U, 0x80000001U,
				0xdeadbeefU, 0x0fedcba9U, 0xcafebabeU, 0x00111100U);
  __m256i d = _mm256_set_epi16 (0xdead, 0xbeef, 0xcafe, 0xbabe,
				0x1234, 0x0012, 0x8001, 0xa55a,
				0x5678, 0x9abc, 0xdef0, 0x1020,
				0x8070, 0x6543, 0x129f, 0);
  s2.a = _mm256_maskz_sll_epi64 (0xc, a, b);
  s2.b = _mm256_maskz_sll_epi32 (0x27, c, b);
  s2.c = _mm256_maskz_sll_epi16 (0x139f, d, b);
  s2.d = _mm256_maskz_srl_epi64 (0x3, a, b);
  s2.e = _mm256_maskz_srl_epi32 (0x9a, c, b);
  s2.f = _mm256_maskz_srl_epi16 (0x79a1, d, b);
  s2.g = _mm256_maskz_sra_epi64 (0xa, a, b);
  s2.h = _mm256_maskz_sra_epi32 (0x19, c, b);
  s2.i = _mm256_maskz_sra_epi16 (0x3acb, d, b);
  s2.j = _mm256_maskz_slli_epi64 (0x7, a, 3);
  s2.k = _mm256_maskz_slli_epi32 (0x7a, c, 5);
  s2.l = _mm256_maskz_slli_epi16 (0x9b1c, d, 6);
  s2.m = _mm256_maskz_srli_epi64 (0x2, a, 7);
  s2.n = _mm256_maskz_srli_epi32 (0x39, c, 8);
  s2.o = _mm256_maskz_srli_epi16 (0xabe1, d, 9);
  s2.p = _mm256_maskz_srai_epi64 (0x9, a, 10);
  s2.q = _mm256_maskz_srai_epi32 (0x17, c, 11);
  s2.r = _mm256_maskz_srai_epi16 (0x19ae, d, 17);
}

/* { dg-final { scan-tree-dump-times "s3.a = \{ 0, 5083102811422187008, 0, -2577231341703702080, -163971058432973792, 0, 0, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.b = \{ 597996886556672, -3046722668750438400, 5083102728347975712, 0, 6906085035741282304, -3046722664493648608, 32, 597996886556672 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.c = \{ -3053196591608342592, 1008991448208506880, -2458860116837924864, 162129588191778752, 1008991446801317888, -3530822104133926912, 5080062853577179136, -3053196593214761024 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.d = \{ 0, 0, 0, 0, 35868669032213017, 40992764608243447, 573943924821290966, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.e = \{ 432346144048152576, 501427841262775799, 40992761372999680, 8351325, 457104182378233856, 501427841146028032, 0, 432346144057696947 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.f = \{ 501313492043104256, 289078280015054423, 429812289518239744, 4398046511104, 289078280015052800, 194499209023914113, 40813871690153984, 501306933628044757 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.g = \{ -75032909037701675, 0, 0, 0, 35868669032213017, 0, 0, -75032909037701675 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.h = \{ -144114608255270912, -75032911157395456, 40992761305890816, 8351325, 34952, -75032911157395456, 4227858432, -144114608255270912 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.i = \{ -74874577215881216, -287382472288305577, 65271, 64981, 3478933209088, 4277600385, 40813875850903552, 279241565863936 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.j = \{ -761680639942076944, 0, 9182379272246532360, 0, 0, 0, 0, -761680639942076944 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.k = \{ 17895424, 0, 16, 0, -5770329518984134656, 4275878544, 0, 298998443278336 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.l = \{ -6106881094714392576, 88788378369920, 210559412731904, 274877951872, 88785563942912, 3154118656, 4194304, 44928 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.m = \{ 0, 10248191152060861, 0, 0, 8967167258053254, 0, 0, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.n = \{ 54043264247922688, 14593470, 8388608, 1043915, 0, 1043915, 71737335514923008, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.o = \{ 31244130443395072, 18014613257846885, 26740556579209216, 6619229, 18014398510071808, 12103423998558216, 2533274790395904, 408028512349 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.p = \{ 0, 0, 0, 0, 1120895907256656, 1281023894007607, -78650858816642, -2344778407428178 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.q = \{ 546, 0, 640508472852480, 0, -1864947814366686, 0, 0, 149130 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.r = \{ 4294901760, 65535, 281470681743360, 281470681808895, 0, 281474976645120, 0, -281474976645121 \};" 1 "optimized" } } */

void
baz (void)
{
  __m512i a = _mm512_set_epi64 (0xdeadbeefcafebabeULL, 0xfee1deadfeedfaceULL,
				0x123456789abcdef0ULL, 0x0fedcba987654321ULL,
				0xfee1deadfeedfaceULL, 0x0fedcba987654321ULL,
				0x123456789abcdef0ULL, 0xdeadbeefcafebabeULL);
  __m128i b = _mm_set1_epi64x (5);
  __m512i c = _mm512_set_epi32 (0xc00010ffU, 0x12345678U, 0xfedcba98U, 0x80000001U,
				0xdeadbeefU, 0x0fedcba9U, 0xcafebabeU, 0x00111100U,
				0, 0x0fedcba9U, 0x12345678U, 0x80000001U,
				0xdeadbeefU, 0xdeadbeefU, 0xc00010ffU, 0x00111100U);
  __m512i d = _mm512_set_epi16 (0xdead, 0xbeef, 0xcafe, 0xbabe,
				0x1234, 0x0012, 0x8001, 0xa55a,
				0x5678, 0x9abc, 0xdef0, 0x1020,
				0x8070, 0x6543, 0x129f, 0,
				0x0012, 0x8001, 0xcafe, 0xbabe,
				0xbeef, 0xcafe, 0x9abc, 0xdef0,
				0x8070, 0x6543, 0x129f, 0xcafe,
				0xdead, 0xbeef, 0xcafe, 0xbabe);
  s3.a = _mm512_maskz_sll_epi64 (0x1a, a, b);
  s3.b = _mm512_maskz_sll_epi32 (0x9eba, c, b);
  s3.c = _mm512_maskz_sll_epi16 (0xdeadbeefU, d, b);
  s3.d = _mm512_maskz_srl_epi64 (0x70, a, b);
  s3.e = _mm512_maskz_srl_epi32 (0xcafe, c, b);
  s3.f = _mm512_maskz_srl_epi16 (0xbabecafeU, d, b);
  s3.g = _mm512_maskz_sra_epi64 (0x91, a, b);
  s3.h = _mm512_maskz_sra_epi32 (0x996a, c, b);
  s3.i = _mm512_maskz_sra_epi16 (0x6a3791feU, d, b);
  s3.j = _mm512_maskz_slli_epi64 (0x85, a, 3);
  s3.k = _mm512_maskz_slli_epi32 (0x8691, c, 4);
  s3.l = _mm512_maskz_slli_epi16 (0x12345678U, d, 6);
  s3.m = _mm512_maskz_srli_epi64 (0x12, a, 7);
  s3.n = _mm512_maskz_srli_epi32 (0x3456, c, 8);
  s3.o = _mm512_maskz_srli_epi16 (0x789abcdeU, d, 9);
  s3.p = _mm512_maskz_srai_epi64 (0xf0, a, 10);
  s3.q = _mm512_maskz_srai_epi32 (0x4321, c, 11);
  s3.r = _mm512_maskz_srai_epi16 (0x98765432U, d, 17);
}
