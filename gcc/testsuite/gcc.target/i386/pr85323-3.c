/* PR target/85323 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512bw -fdump-tree-optimized" } */

#include <x86intrin.h>

struct S1 { __m128i a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r; } s1;
struct S2 { __m256i a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r; } s2;
struct S3 { __m512i a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r; } s3;

/* { dg-final { scan-tree-dump-times "s1.a = \{ -644307835425925520, -761680639942076944 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.b = \{ -655884249613729784, 149501664998336 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.c = \{ -7953356323460470064, -763088040595761680 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.d = \{ 2295775699285163865, 2005711373062887255 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.e = \{ 2295594818081914880, 1729384589115689679 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.f = \{ 163818445303977131, 2005535447444297559 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.g = \{ -10067309928530087, -300131636150806697 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.h = \{ -10248187373682688, -576458420098004273 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.i = \{ 163818449062130859, -300061267406620841 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.j = \{ -1288615670851851040, -1523361279884153888 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.k = \{ -2623536998454919136, 597998070058752 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.l = \{ -8286618366555171200, -6106674658193395840 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.m = \{ 143485981205322741, 125356960816430453 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.n = \{ 71737335514923008, 54043264249115734 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.o = \{ 2533274794590290, 31244130443395165 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.p = \{ -78650858816642, -2344778407428178 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.q = \{ -40029096247296, -2251791223601526 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.r = \{ 4294967295, -1 \};" 1 "optimized" } } */

void
foo (void)
{
  __m128i a = _mm_set_epi64x (0xdeadbeefcafebabeULL, 0xfee1deadfeedfaceULL);
  __m128i b = _mm_set1_epi64x (3);
  __m128i c = _mm_set_epi32 (0xc00010ffU, 0x12345678U, 0xfedcba98U, 0x80000001U);
  __m128i d = _mm_set_epi16 (0xdead, 0xbeef, 0xcafe, 0xbabe,
			     0x1234, 0x0012, 0x8001, 0xa55a);
  s1.a = _mm_sll_epi64 (a, b);
  s1.b = _mm_sll_epi32 (c, b);
  s1.c = _mm_sll_epi16 (d, b);
  s1.d = _mm_srl_epi64 (a, b);
  s1.e = _mm_srl_epi32 (c, b);
  s1.f = _mm_srl_epi16 (d, b);
  s1.g = _mm_sra_epi64 (a, b);
  s1.h = _mm_sra_epi32 (c, b);
  s1.i = _mm_sra_epi16 (d, b);
  s1.j = _mm_slli_epi64 (a, 4);
  s1.k = _mm_slli_epi32 (c, 5);
  s1.l = _mm_slli_epi16 (d, 6);
  s1.m = _mm_srli_epi64 (a, 7);
  s1.n = _mm_srli_epi32 (c, 8);
  s1.o = _mm_srli_epi16 (d, 9);
  s1.p = _mm_srai_epi64 (a, 10);
  s1.q = _mm_srai_epi32 (c, 11);
  s1.r = _mm_srai_epi16 (d, 17);
}

/* { dg-final { scan-tree-dump-times "s2.a = \{ -81985529216486896, 2541551405711093504, -1288615670851851040, -1523361279884153888 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.b = \{ -5770329518966239232, -1523361330099340656, -1311768499227459568, 298999035029376 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.c = \{ 504495724104253440, 7458149828057367040, 2540031426788611488, -1526457556168299552 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.d = \{ 71737338064426034, 81985529216486895, 1147887849642581932, 1002855686531443627 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.e = \{ 914208369051504912, 1002855682308758714, 1147797406893473792, 864692292410361191 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.f = \{ 578438035006881792, 389290523068662018, 81909222651988565, 1002626984086277035 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.g = \{ 71737338064426034, 81985529216486895, -5033654964265044, -150065818075403349 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.h = \{ -238713135555342064, -150065822298088262, -5124093686841344, -288229212196485785 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.i = \{ -574483469599965184, 389554409885860098, 81909226678581845, -150030633703310421 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.j = \{ 9182379272246532360, -7952596333999229056, -644307835425925520, -761680639942076944 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.k = \{ 6906085035777073152, -3046722664493648608, -2623536998454919136, 597998070058752 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.l = \{ 2017701421440303104, -7061451798027958272, -8286618366555171200, -6106674658193395840 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.m = \{ 8967167258053254, 10248191152060861, 143485981205322741, 125356960816430453 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.n = \{ 57138020112929041, 62678476386201035, 71737335514923008, 54043264249115734 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.o = \{ 18014613258436608, 12103754718314504, 2533274794590290, 31244130443395165 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.p = \{ 1120895907256656, 1281023894007607, -78650858816642, -2344778407428178 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.q = \{ -1864947814366686, -1172392927691335, -40029096247296, -2251791223601526 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.r = \{ -281474976710656, 281474976645120, 4294967295, -1 \};" 1 "optimized" } } */
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
  s2.a = _mm256_sll_epi64 (a, b);
  s2.b = _mm256_sll_epi32 (c, b);
  s2.c = _mm256_sll_epi16 (d, b);
  s2.d = _mm256_srl_epi64 (a, b);
  s2.e = _mm256_srl_epi32 (c, b);
  s2.f = _mm256_srl_epi16 (d, b);
  s2.g = _mm256_sra_epi64 (a, b);
  s2.h = _mm256_sra_epi32 (c, b);
  s2.i = _mm256_sra_epi16 (d, b);
  s2.j = _mm256_slli_epi64 (a, 3);
  s2.k = _mm256_slli_epi32 (c, 5);
  s2.l = _mm256_slli_epi16 (d, 6);
  s2.m = _mm256_srli_epi64 (a, 7);
  s2.n = _mm256_srli_epi32 (c, 8);
  s2.o = _mm256_srli_epi16 (d, 9);
  s2.p = _mm256_srai_epi64 (a, 10);
  s2.q = _mm256_srai_epi32 (c, 11);
  s2.r = _mm256_srai_epi16 (d, 17);
}

/* { dg-final { scan-tree-dump-times "s3.a = \{ -3046722559768307776, 5083102811422187008, -163971058432973792, -2577231341703702080, -163971058432973792, 5083102811422187008, -2577231341703702080, -3046722559768307776 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.b = \{ 597996922347520, -3046722665164841504, 5083102728347975712, 4256789792, 6906085035777073152, -3046722664493648608, -2623536998454919136, 597998070058752 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.c = \{ -3053196591608342592, 1008991448208531392, -2458860116837868032, 162129725630732224, 1008991448208506880, -3530725896866495488, 5080062853577222976, -3053196591608342592 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.d = \{ 501427843265721813, 40992764608243447, 35868669032213017, 573943924821290966, 35868669032213017, 40992764608243447, 573943924821290966, 501427843265721813 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.e = \{ 432346144048187528, 501427841262775799, 40992761372999680, 8351325, 457104182378268808, 501427841154379357, 573898701299253248, 432346144057696947 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.f = \{ 501313492043105749, 289078280015054423, 429819260250162935, 4398152877525, 289078280015052800, 194504521898459265, 40813871690155306, 501313492043105749 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.g = \{ -75032909037701675, 40992764608243447, 35868669032213017, -2516827482132522, 35868669032213017, 40992764608243447, -2516827482132522, -75032909037701675 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.h = \{ -144114608255235960, -75032906879898121, 40992765533749248, 8351325, -119356569925154680, -75032911149044131, -2562046843420672, -144114608245726541 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.i = \{ -74874577215816235, -287382472288305577, -146368809008759049, 277081197379029, -287382472288370688, 194777204942897281, 40813875850968362, -74874577215816235 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.j = \{ -761680639942076944, -7952596333999229056, 9182379272246532360, -644307835425925520, 9182379272246532360, -7952596333999229056, -644307835425925520, -761680639942076944 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.k = \{ 298998461173760, -1523361330434937104, 2541551364173987856, 4275878544, -5770329518966239232, -1523361330099340656, -1311768499227459568, 298999035029376 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.l = \{ -6106674658193395840, 2017701421440352128, -4917720233675801600, 324259451261464448, 2017701421440303104, -7061451798027958272, -8286618366555171200, -6106674658193395840 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.m = \{ 125356960816430453, 10248191152060861, 8967167258053254, 143485981205322741, 8967167258053254, 10248191152060861, 143485981205322741, 125356960816430453 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.n = \{ 54043264247927057, 62678476399750590, 5124093561012224, 1043915, 57138020112929041, 62678476386201035, 71737335514923008, 54043264249115734 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.o = \{ 31244130443395165, 18014613258436709, 26740556584255599, 274884526173, 18014613258436608, 12103754718314504, 2533274794590290, 31244130443395165 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.p = \{ -2344778407428178, 1281023894007607, 1120895907256656, -78650858816642, 1120895907256656, 1281023894007607, -78650858816642, -2344778407428178 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.q = \{ -2251791223750110, -1172388633127497, 640512766771200, 130489, -1864947814366686, -1172392927691335, -40029096247296, -2251791223601526 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.r = \{ -1, -281474976645121, -1, 281474976710655, -281474976710656, 281474976645120, 4294967295, -1 \};" 1 "optimized" } } */

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
  s3.a = _mm512_sll_epi64 (a, b);
  s3.b = _mm512_sll_epi32 (c, b);
  s3.c = _mm512_sll_epi16 (d, b);
  s3.d = _mm512_srl_epi64 (a, b);
  s3.e = _mm512_srl_epi32 (c, b);
  s3.f = _mm512_srl_epi16 (d, b);
  s3.g = _mm512_sra_epi64 (a, b);
  s3.h = _mm512_sra_epi32 (c, b);
  s3.i = _mm512_sra_epi16 (d, b);
  s3.j = _mm512_slli_epi64 (a, 3);
  s3.k = _mm512_slli_epi32 (c, 4);
  s3.l = _mm512_slli_epi16 (d, 6);
  s3.m = _mm512_srli_epi64 (a, 7);
  s3.n = _mm512_srli_epi32 (c, 8);
  s3.o = _mm512_srli_epi16 (d, 9);
  s3.p = _mm512_srai_epi64 (a, 10);
  s3.q = _mm512_srai_epi32 (c, 11);
  s3.r = _mm512_srai_epi16 (d, 17);
}
