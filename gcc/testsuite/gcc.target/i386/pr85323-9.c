/* PR target/85323 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512bw -fdump-tree-optimized" } */

#include <x86intrin.h>

struct S1 { __m128i a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r; } s1;
struct S2 { __m256i a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r; } s2;
struct S3 { __m512i a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r; } s3;

/* { dg-final { scan-tree-dump-times "s1.a = \{ -1288052729488364305, -761680639942076944 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.b = \{ -1288052733479223296, -1522798337177157632 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.c = \{ 2623605017854121320, -1522798337177100288 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.d = \{ 35871495301330685, -1522798334225634325 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.e = \{ 4007636207, 1729384592029035499 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.f = \{ 656094930711653615, 2005509212014947307 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.g = \{ -1288052729488364305, -1522798334225634325 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.h = \{ -287331089, -576458420136181760 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.i = \{ 656094934652136687, -1522798334225612805 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.j = \{ -4342213319840130048, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.k = \{ 0, 149499221639168 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.l = \{ 2623346860252725248, 57344 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.m = \{ 0, 2005711373062887255 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.n = \{ 128, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.o = \{ 655836695802937344, 2005509209063424011 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.p = \{ -157301717633283, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.q = \{ 4294967168, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s1.r = \{ 655836695735888214, 4294967291 \};" 1 "optimized" } } */

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
  __m128i g = _mm_set_epi64x (0xeaddeefbafecabebULL, 0xee1feaddeedfacefULL);
  s1.a = _mm_mask_sllv_epi64 (g, 0x2, a, b);
  s1.b = _mm_mask_sllv_epi32 (g, 0x5, c, d);
  s1.c = _mm_mask_sllv_epi16 (g, 0x3b, e, f);
  s1.d = _mm_mask_srlv_epi64 (g, 0x1, a, b);
  s1.e = _mm_mask_srlv_epi32 (g, 0xa, c, d);
  s1.f = _mm_mask_srlv_epi16 (g, 0xca, e, f);
  s1.g = _mm_mask_srav_epi64 (g, 0, a, b);
  s1.h = _mm_mask_srav_epi32 (g, 0xe, c, d);
  s1.i = _mm_mask_srav_epi16 (g, 0x18, e, f);
  s1.j = _mm_maskz_sllv_epi64 (0x1, a, b);
  s1.k = _mm_maskz_sllv_epi32 (0xa, c, d);
  s1.l = _mm_maskz_sllv_epi16 (0x7c, e, f);
  s1.m = _mm_maskz_srlv_epi64 (0x2, a, b);
  s1.n = _mm_maskz_srlv_epi32 (0x7, c, d);
  s1.o = _mm_maskz_srlv_epi16 (0x9a, e, f);
  s1.p = _mm_maskz_srav_epi64 (0x1, a, b);
  s1.q = _mm_maskz_srav_epi32 (0x5, c, d);
  s1.r = _mm_maskz_srav_epi16 (0x39, e, f);
}

/* { dg-final { scan-tree-dump-times "s2.a = \{ 3771334344009719049, -2171106659920117469, 0, -1523361279884153888 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.b = \{ 9177596069264525312, -2171106662061094912, -81985527194080017, -1522798334225634325 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.c = \{ 1008895103428722688, -5985092594349506048, -1288052733493865112, -1522798334225620992 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.d = \{ 2189249818860, 0, -1288052729488364305, -1522798334225634325 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.e = \{ 114276047740869897, 3992584483, -1288052733496000512, 3377706967018475 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.f = \{ 3771334344009711616, 3115556349134373155, 656094934652102998, -1523061098981359605 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.g = \{ 3771334344009719049, 0, -1288052729488364305, -150065818075403349 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.h = \{ 3771334340789805600, -302382813, -1288052729201033217, -1522798334225634325 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.i = \{ -287253348398858240, 3115618685519790371, 656094934872403183, -1522798332882190341 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.j = \{ 6722813395751927808, 0, 0, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.k = \{ 9177596069255577600, 0, -81985531201716224, 610839792 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.l = \{ 1008895102094934016, 512, 2623346782945449320, -763360136839241728 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.m = \{ 0, 0, 0, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.n = \{ 139808, 0, -81985531201716224, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.o = \{ 0, 3115407575762206720, 655836695802947926, 2005509209063424000 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.p = \{ 2189249818860, 0, 0, -150065818075403349 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.q = \{ -29839143554899424, 130489, 4294967295, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s2.r = \{ -287384211757400064, 3115365046459170816, 655836695735888214, 65531 \};" 1 "optimized" } } */

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
  __m256i g = _mm256_set_epi64x (0xeaddeefbafecabebULL, 0xee1feaddeedfacefULL,
				 0xe1deadfeedfa0123ULL, 0x3456789abfee1d09ULL);
  s2.a = _mm256_mask_sllv_epi64 (g, 0xc, a, b);
  s2.b = _mm256_mask_sllv_epi32 (g, 0x27, c, d);
  s2.c = _mm256_mask_sllv_epi16 (g, 0x139f, e, f);
  s2.d = _mm256_mask_srlv_epi64 (g, 0x3, a, b);
  s2.e = _mm256_mask_srlv_epi32 (g, 0x9a, c, d);
  s2.f = _mm256_mask_srlv_epi16 (g, 0x79a1, e, f);
  s2.g = _mm256_mask_srav_epi64 (g, 0xa, a, b);
  s2.h = _mm256_mask_srav_epi32 (g, 0x19, c, d);
  s2.i = _mm256_mask_srav_epi16 (g, 0x3acb, e, f);
  s2.j = _mm256_maskz_sllv_epi64 (0x7, a, b);
  s2.k = _mm256_maskz_sllv_epi32 (0x7a, c, d);
  s2.l = _mm256_maskz_sllv_epi16 (0x9b1c, e, f);
  s2.m = _mm256_maskz_srlv_epi64 (0x2, a, b);
  s2.n = _mm256_maskz_srlv_epi32 (0x39, c, d);
  s2.o = _mm256_maskz_srlv_epi16 (0xabe1, e, f);
  s2.p = _mm256_maskz_srav_epi64 (0x9, a, b);
  s2.q = _mm256_maskz_srav_epi32 (0x17, c, d);
  s2.r = _mm256_maskz_srav_epi16 (0x19ae, e, f);
}

/* { dg-final { scan-tree-dump-times "s3.a = \{ -7817839361497542344, 5247073869855161280, -2401053089206453570, -161076958856481380, 6722813395751927808, -2171106659920117469, -1288052729488364305, -1522813727388423189 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.b = \{ -4611667328963227336, 6592669524527332030, 2541551364173987968, 3989547399, 9177596072475630857, 1851607040, -1288052733496000512, 76543604430777323 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.c = \{ 6233191819462621886, 8070591269736331966, 8610979175836203710, 40756324093949308, 1008895105314979840, -5985092594189729501, 2623346860254866671, -763360133887762432 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.d = \{ -7817839361497542344, -80538480299558210, -2401053089206453570, 1147797410748803463, 2189249818860, 0, 0, -1522813727388423189 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.e = \{ -4611667328963227336, 31339239126560699, 81985526923526144, 66810602, 114276047740869897, 3992584483, -1288052729488364305, 3377704168205116 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.f = \{ 125466298768380216, 36028797018976959, 107451899833268926, 563228820875655, 289076540546030857, 3115556349134373122, 656094930711653615, 2005756577704837131 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.g = \{ -4689556814856355, -80538480299558210, -2401053089206453570, 1147797410748803463, 2189249818860, -2171106659920117469, -1288052729488364305, -150065818075403349 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.h = \{ -4611667328963227336, -4689555420693826, 81985530312440510, 1147797406826066666, 3771334340789805600, -302382813, -1288052729201033217, -1125892660352021 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.i = \{ -18648885549379272, -36028797018901825, -2401053089206435841, 786880652893535, 3771203477433548800, -2171106659687071486, 656094934872403183, -1522779624004670485 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.j = \{ 0, 5247073869855161280, 0, -161076958856481380, 6722813395751927808, 0, 0, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.k = \{ -4611667331024683008, 6592669521121640448, 2541551364173987968, 0, 9177596069255577600, 1851607040, 0, 76543601479254016 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.l = \{ 6233191819462621886, 8070591269736284160, 8610979175836155904, 40532397384037756, 1008895102094934016, -5985283900623028224, 2623346860254822400, -763360136839241728 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.m = \{ 0, 0, 0, 0, 2189249818860, 0, 0, 0 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.n = \{ -4611667331024683008, 31339239126560699, 81985526923526144, 66810602, 114276044520816640, 0, 0, 3377704168205116 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.o = \{ 125466298768359424, 36028797018976959, 107241966126759936, 563224831328256, 289076540546023424, 3115365042701074690, 655836695802937344, 2005509209063424011 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.p = \{ -4689556814856355, 0, 0, 0, 2189249818860, 0, 0, -150065818075403349 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.q = \{ -4611667331024683008, -4689558826385408, 81985526906748928, 66810602, 139808, -4294967296, 4294967295, -1125895611875328 \};" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "s3.r = \{ -18648885549400064, -36028797018901825, 65535, 562949953477983, 1739464179712, 4225630466, 655836699963686912, 281474976645120 \};" 1 "optimized" } } */

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
  __m512i g = _mm512_set_epi64 (0xeadde0fbafecabebULL, 0xee1feaddeedfacefULL,
				0xe1deadfeedfa0123ULL, 0x3456789abfee1d09ULL,
				0x0fedcba9edcba987ULL, 0xdeadbeefcafebabeULL,
				0xfee1deadcafebabeULL, 0x938174857adf5138ULL);
  s3.a = _mm512_mask_sllv_epi64 (g, 0x1a, a, b);
  s3.b = _mm512_mask_sllv_epi32 (g, 0x9eba, c, d);
  s3.c = _mm512_mask_sllv_epi16 (g, 0xdeadbeefU, e, f);
  s3.d = _mm512_mask_srlv_epi64 (g, 0x70, a, b);
  s3.e = _mm512_mask_srlv_epi32 (g, 0xcafe, c, d);
  s3.f = _mm512_mask_srlv_epi16 (g, 0xbabecafeU, e, f);
  s3.g = _mm512_mask_srav_epi64 (g, 0x91, a, b);
  s3.h = _mm512_mask_srav_epi32 (g, 0x996a, c, d);
  s3.i = _mm512_mask_srav_epi16 (g, 0x6a3791feU, e, f);
  s3.j = _mm512_maskz_sllv_epi64 (0x1a, a, b);
  s3.k = _mm512_maskz_sllv_epi32 (0x9eba, c, d);
  s3.l = _mm512_maskz_sllv_epi16 (0xdeadbeefU, e, f);
  s3.m = _mm512_maskz_srlv_epi64 (0x70, a, b);
  s3.n = _mm512_maskz_srlv_epi32 (0xcafe, c, d);
  s3.o = _mm512_maskz_srlv_epi16 (0xbabecafeU, e, f);
  s3.p = _mm512_maskz_srav_epi64 (0x91, a, b);
  s3.q = _mm512_maskz_srav_epi32 (0x996a, c, d);
  s3.r = _mm512_maskz_srav_epi16 (0x6a3791feU, e, f);
}
