/* PR target/85323 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512bw -fdump-tree-optimized" } */

#include <x86intrin.h>

struct S1 { __m128i a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r; } s1;
struct S2 { __m256i a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r; } s2;
struct S3 { __m512i a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r; } s3;

/* { dg-final { scan-tree-dump-times "s1.a = \{ -1288052729488364305, -761680639942076944 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.b = \{ -1288052733496000504, -1522798334733798464 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.c = \{ -7953098707027088688, -1522798335701756432 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.d = \{ 2295775699285163865, -1522798334225634325 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.e = \{ 2295594821821115631, 1729384592029035499 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.f = \{ 164076671622753519, 2005535449970158571 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.g = \{ -1288052729488364305, -1522798334225634325 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.h = \{ -10248187392578321, -576458420098004273 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.i = \{ 164076675361909999, -1522798334225615017 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.j = \{ -1288615670851851040, -1522798334225634325 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.k = \{ -2623536994447282961, 597999838079979 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.l = \{ -8286618362551751441, -1522854662460362880 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.m = \{ -1288052729488364305, 125356960816430453 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.n = \{ 71737335514923008, -1522798337175964586 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.o = \{ 2791509703306479, 31506487170564189 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.p = \{ -78650858816642, -1522798334225634325 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.q = \{ -1288052729202081792, -1522798337177008502 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.r = \{ 258238916329471, -1522798332882190337 \};" 1 "optimized" } } */

void
foo (void)
{
  __m128i a = _mm_set_epi64x (0xdeadbeefcafebabeULL, 0xfee1deadfeedfaceULL);
  __m128i b = _mm_set1_epi64x (3);
  __m128i c = _mm_set_epi32 (0xc00010ffU, 0x12345678U, 0xfedcba98U, 0x80000001U);
  __m128i d = _mm_set_epi16 (0xdead, 0xbeef, 0xcafe, 0xbabe,
			     0x1234, 0x0012, 0x8001, 0xa55a);
  __m128i e = _mm_set_epi64x (0xeaddeefbafecabebULL, 0xee1feaddeedfacefULL);
  s1.a = _mm_mask_sll_epi64 (e, 0x2, a, b);
  s1.b = _mm_mask_sll_epi32 (e, 0x5, c, b);
  s1.c = _mm_mask_sll_epi16 (e, 0x3b, d, b);
  s1.d = _mm_mask_srl_epi64 (e, 0x1, a, b);
  s1.e = _mm_mask_srl_epi32 (e, 0xa, c, b);
  s1.f = _mm_mask_srl_epi16 (e, 0xca, d, b);
  s1.g = _mm_mask_sra_epi64 (e, 0, a, b);
  s1.h = _mm_mask_sra_epi32 (e, 0xe, c, b);
  s1.i = _mm_mask_sra_epi16 (e, 0x18, d, b);
  s1.j = _mm_mask_slli_epi64 (e, 0x1, a, 4);
  s1.k = _mm_mask_slli_epi32 (e, 0xa, c, 5);
  s1.l = _mm_mask_slli_epi16 (e, 0x7c, d, 6);
  s1.m = _mm_mask_srli_epi64 (e, 0x2, a, 7);
  s1.n = _mm_mask_srli_epi32 (e, 0x7, c, 8);
  s1.o = _mm_mask_srli_epi16 (e, 0x9a, d, 9);
  s1.p = _mm_mask_srai_epi64 (e, 0x1, a, 10);
  s1.q = _mm_mask_srai_epi32 (e, 0x5, c, 11);
  s1.r = _mm_mask_srai_epi16 (e, 0x39, d, 17);
}

/* { dg-final { scan-tree-dump-times "s2.a = \{ 3771334344009719049, -2171106659920117469, -1288615670851851040, -1523361279884153888 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.b = \{ -5770329518966239232, -2171106659636823408, -1311768495219823377, -1522798334225634325 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.c = \{ 504495724104253440, 7458152293351424512, -1288052733494930016, -1522798334225634336 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.d = \{ 71737338064426034, 81985529216486895, -1288052729488364305, -1522798334225634325 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.e = \{ 914208372271488265, 1002855686284640547, -1288052733361782784, 864692295342795755 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.f = \{ 3771334344009711616, 389471199457902883, 82167457139067477, -1523047981938439253 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.g = \{ 3771334344009719049, 81985529216486895, -1288052729488364305, -150065818075403349 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.h = \{ 3771334340789735696, -150065818322206429, -1288052729335250944, -1522798334225634325 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.i = \{ -574357824626688000, 389554409618145571, 82167457292266735, -1522798332937765973 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.j = \{ 9182379272246532360, -7952596333999229056, -644307835425925520, -1522798334225634325 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.k = \{ 6906085038961335561, -3046722664757853917, -2623536998454919136, -1522798335993655552 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.l = \{ 2017701421845978377, -2171106659920115712, -8286365079448824192, -6106618329958666368 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.m = \{ 3771334344009719049, 10248191152060861, -1288052729488364305, -1522798334225634325 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.n = \{ 3771334340789670161, 62678480377741603, 71737335514923008, -1522798334225634325 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.o = \{ 3771334344009711616, 12103754718314787, 2791509703262290, 31506484225747947 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.p = \{ 1120895907256656, -2171106659920117469, -1288052729488364305, -2344778407428178 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.q = \{ -1864947814366686, -2171106663912571463, -1288052729202081792, -1522798334225634325 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.r = \{ -281474976703223, 191310728200483, 258238916329471, -1522798334225612801 \};" 1 "optimized" } } */

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
  __m256i e = _mm256_set_epi64x (0xeaddeefbafecabebULL, 0xee1feaddeedfacefULL,
				 0xe1deadfeedfa0123ULL, 0x3456789abfee1d09ULL);
  s2.a = _mm256_mask_sll_epi64 (e, 0xc, a, b);
  s2.b = _mm256_mask_sll_epi32 (e, 0x27, c, b);
  s2.c = _mm256_mask_sll_epi16 (e, 0x139f, d, b);
  s2.d = _mm256_mask_srl_epi64 (e, 0x3, a, b);
  s2.e = _mm256_mask_srl_epi32 (e, 0x9a, c, b);
  s2.f = _mm256_mask_srl_epi16 (e, 0x79a1, d, b);
  s2.g = _mm256_mask_sra_epi64 (e, 0xa, a, b);
  s2.h = _mm256_mask_sra_epi32 (e, 0x19, c, b);
  s2.i = _mm256_mask_sra_epi16 (e, 0x3acb, d, b);
  s2.j = _mm256_mask_slli_epi64 (e, 0x7, a, 3);
  s2.k = _mm256_mask_slli_epi32 (e, 0x7a, c, 5);
  s2.l = _mm256_mask_slli_epi16 (e, 0x9b1c, d, 6);
  s2.m = _mm256_mask_srli_epi64 (e, 0x2, a, 7);
  s2.n = _mm256_mask_srli_epi32 (e, 0x39, c, 8);
  s2.o = _mm256_mask_srli_epi16 (e, 0xabe1, d, 9);
  s2.p = _mm256_mask_srai_epi64 (e, 0x9, a, 10);
  s2.q = _mm256_mask_srai_epi32 (e, 0x17, c, 11);
  s2.r = _mm256_mask_srai_epi16 (e, 0x19ae, d, 17);
}

/* { dg-final { scan-tree-dump-times "s3.a = \{ -7817839361497542344, 5083102811422187008, -2401053089206453570, -2577231341703702080, -163971058432973792, -2171106659920117469, -1288052729488364305, -1522813727388423189 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.b = \{ 597998948012344, -3046722665344746818, 5083102728347975712, 3989547399, 6906085038961335561, -3046722664493648608, -1288052733496000480, 597999838079979 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.c = \{ -3053196591608342592, 1008991448208554686, -2458860116837877058, 162353514901690304, 1008991450021363712, -3530630797700628189, 5080062853577223407, -3053196590263281728 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.d = \{ -7817839361497542344, -80538480299558210, -2401053089206453570, 1147797410748803463, 35868669032213017, 40992764608243447, 573943924821290966, -1522813727388423189 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.e = \{ 432346146109608248, 501427841262775799, 40992761372999680, 8351325, 457104185598287113, 501427845138612515, -1288052729488364305, 432346144057696947 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.f = \{ 501313492043125048, 289078280015054423, 430022223224748734, 4402036058503, 289078280015060233, 194690515457212545, 41072106598870255, 501554302269457877 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.g = \{ -75032909037701675, -80538480299558210, -2401053089206453570, 1147797410748803463, 35868669032213017, -2171106659920117469, -1288052729488364305, -75032909037701675 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.h = \{ -144114606193815240, -75032907751703874, 40992764711582398, 1147797406767607389, 3771334340789700744, -75032907164810973, -1288052729268142080, -144114605303747605 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.i = \{ -74874577215860424, -287382472288305577, -2401053089206436105, 223930699480533, 3771205216902578176, -2171106659635101567, 41072110759619823, -1522781857415451669 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.j = \{ -761680639942076944, -80538480299558210, 9182379272246532360, 1147797410748803463, 3771334344009719049, -2171106659920117469, -1288052729488364305, -761680639942076944 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.k = \{ -7817839363541102592, -80538480299558210, -2401053092612145136, 3989547399, -5770329515764081399, -2171106659636823408, -1288052729488364305, 299001394801643 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.l = \{ -6106752978073464520, -80694529937588352, -2401052466905826626, 1147573758916800384, 3771290526753365257, -2171106660758583296, -1288052733491761937, -1522813727388422272 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.m = \{ -7817839361497542344, 10248191152060861, -2401053089206453570, 1147797410748803463, 8967167258053254, -2171106659920117469, -1288052729488364305, -1522813727388423189 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.n = \{ 54043266309378360, -80538483690656322, -2401053092603756544, 1147797406760299979, 3771334344009719049, -2171106663911658037, 71737335514923008, -1522813727388423189 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.o = \{ 31244130443415864, 18014616663490661, 26740559984900798, 223926716530781, 18147001330375945, 12294734424440840, 2791513706704111, -1523060690952847267 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.p = \{ -7817839361497542344, -80538480299558210, -2401053089206453570, 1147797410748803463, 1120895907256656, 1281023894007607, -78650858816642, -2344778407428178 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.q = \{ -7817839363558997470, -80538480299558210, 640511878544062, 1147797410748803463, -1864947814366686, -2171106659920117469, -1288052729488364305, -1522813730339797366 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.r = \{ -7817839359264075464, -80538483705184257, -2400981552231171394, 1147854954720657407, 3771201737969376521, -2171016495369355264, 258238916308207, -34103383752705 \};" 1 "optimized" } } */

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
  __m512i e = _mm512_set_epi64 (0xeadde0fbafecabebULL, 0xee1feaddeedfacefULL,
				0xe1deadfeedfa0123ULL, 0x3456789abfee1d09ULL,
				0x0fedcba9edcba987ULL, 0xdeadbeefcafebabeULL,
				0xfee1deadcafebabeULL, 0x938174857adf5138ULL);
  s3.a = _mm512_mask_sll_epi64 (e, 0x1a, a, b);
  s3.b = _mm512_mask_sll_epi32 (e, 0x9eba, c, b);
  s3.c = _mm512_mask_sll_epi16 (e, 0xdeadbeefU, d, b);
  s3.d = _mm512_mask_srl_epi64 (e, 0x70, a, b);
  s3.e = _mm512_mask_srl_epi32 (e, 0xcafe, c, b);
  s3.f = _mm512_mask_srl_epi16 (e, 0xbabecafeU, d, b);
  s3.g = _mm512_mask_sra_epi64 (e, 0x91, a, b);
  s3.h = _mm512_mask_sra_epi32 (e, 0x996a, c, b);
  s3.i = _mm512_mask_sra_epi16 (e, 0x6a3791feU, d, b);
  s3.j = _mm512_mask_slli_epi64 (e, 0x85, a, 3);
  s3.k = _mm512_mask_slli_epi32 (e, 0x8691, c, 4);
  s3.l = _mm512_mask_slli_epi16 (e, 0x12345678U, d, 6);
  s3.m = _mm512_mask_srli_epi64 (e, 0x12, a, 7);
  s3.n = _mm512_mask_srli_epi32 (e, 0x3456, c, 8);
  s3.o = _mm512_mask_srli_epi16 (e, 0x789abcdeU, d, 9);
  s3.p = _mm512_mask_srai_epi64 (e, 0xf0, a, 10);
  s3.q = _mm512_mask_srai_epi32 (e, 0x4321, c, 11);
  s3.r = _mm512_mask_srai_epi16 (e, 0x98765432U, d, 17);
}
