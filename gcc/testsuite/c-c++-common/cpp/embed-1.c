/* { dg-do run } */
/* { dg-options "--embed-dir=${srcdir}/c-c++-common/cpp/embed-dir" } */
/* { dg-additional-options "-std=gnu99" { target c } } */

#if !defined(__STDC_EMBED_NOT_FOUND__) || __STDC_EMBED_NOT_FOUND__ != 0
#error "__STDC_EMBED_NOT_FOUND__ not defined or has invalid value"
#endif

#if __STDC_EMBED_FOUND__ != 1
#error "__STDC_EMBED_FOUND__ not defined or has invalid value"
#endif

#if __STDC_EMBED_EMPTY__ != 2
#error "__STDC_EMBED_EMPTY__ not defined or has invalid value"
#endif

#ifndef __has_embed
#error "__has_embed not defined"
#endif

#if __has_embed (__FILE__) != __STDC_EMBED_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed (__FILE__ limit (NONEXISTENT_MACRO) prefix (1 2 3) __suffix__ () if_empty ({})) != __STDC_EMBED_EMPTY__
#error "__has_embed fail"
#endif

#if __has_embed (__FILE__ __limit__ (1) __prefix__ () suffix (1 / 0) __if_empty__ ((({{[0[0{0{0(0(0)1)1}1}]]}})))) != __STDC_EMBED_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed (__FILE__ limit (0) gnu::nonexistent ({})) != __STDC_EMBED_NOT_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed (__FILE__ limit (0) __gnu__::__non_existent__ ({})) != __STDC_EMBED_NOT_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed (__FILE__ limit (0) gnu::__non_existent__ ({})) != __STDC_EMBED_NOT_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed (__FILE__ limit (0) non_existent_standard_arg (42)) != __STDC_EMBED_NOT_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed ("non-existent-file" limit (NONEXISTENT_MACRO + 1)) != __STDC_EMBED_NOT_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed ("embed-1.c" limit (0)) != __STDC_EMBED_EMPTY__
#error "__has_embed fail"
#endif

#define E1 "embed-1.c"
#define E2 limit (
#define E3 1)
#if __has_embed (E1 E2 E3) != __STDC_EMBED_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed (<embed-1.c>) != __STDC_EMBED_NOT_FOUND__
#error "__has_embed fail"
#endif

#define E4 <embed-1.c>
#define E5 limit
#define E6 1)
#if __has_embed ( \
E4 \
E5 ( \
E6 \
) != __STDC_EMBED_NOT_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed ("embed-1.inc") != __STDC_EMBED_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed ( "embed-1.inc" __limit__ ( 7 - 7 ) ) != __STDC_EMBED_EMPTY__
#error "__has_embed fail"
#endif

#if __has_embed (<embed-1.inc>) != __STDC_EMBED_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed (<embed-1.inc> limit(0)) != __STDC_EMBED_EMPTY__
#error "__has_embed fail"
#endif

#if __has_embed ("../empty.h") != __STDC_EMBED_EMPTY__
#error "__has_embed fail"
#endif

#if __has_embed (<../empty.h>) != __STDC_EMBED_NOT_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed ("../../empty.h") != __STDC_EMBED_EMPTY__
#error "__has_embed fail"
#endif

#if __has_embed (<../../empty.h>) != __STDC_EMBED_EMPTY__
#error "__has_embed fail"
#endif

#if __has_embed ("../empty.h" __limit__ (42)) !=  __STDC_EMBED_EMPTY__
#error "__has_embed fail"
#endif

#if __has_embed (<../empty.h> __limit__ (42)) !=  __STDC_EMBED_NOT_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed ("../../empty.h" limit (42)) !=  __STDC_EMBED_EMPTY__
#error "__has_embed fail"
#endif

#if __has_embed (<../../empty.h> __limit__ (42)) !=  __STDC_EMBED_EMPTY__
#error "__has_embed fail"
#endif

#if 1 + (2 + (3 + (4 + (5 + (6 + (7 + (8 + (9 + (10 + (11 + (12 + (13 + (14 + \
    (15 + (16 + (17 + (18 + (19 + (20 + (21 + (22 + (23 + (24 + (25 + (26 + \
    __has_embed (<magna-carta.txt> limit (17 + (16 + (15 + (14 + (13 \
    + 12))))))))))))))))))))))))))))))) != 27 * 26 / 2 + __STDC_EMBED_FOUND__
#error "__has_embed fail"
#endif

#if 0
#if __has_embed (<non-existent-file.bin> limit (1 + 2 + 3 + 4)) != 42
#error "__has_embed fail"
#endif
#endif

#if (0 && __has_embed (<non-existent-file.bin> limit (3))) != 0
#error "fail"
#endif

#if __STDC_VERSION__ >= 202311L || __cplusplus >= 201103L
static_assert (
#embed <embed-1.inc>
== 'a', "");
static_assert (
#embed "embed-1.inc" \
__limit__ (\
0) __if_empty__ (42) __prefix__ ( \
256 + ) __suffix__ (+ 17)
== 42, "");
static_assert (
#embed "embed-1.inc" limit (42) if_empty (42) prefix (256 + ) suffix (+ 17)
== 256 + 'a' + 17, "");
#endif

int a =
#embed __FILE__ limit (1)
;
int b =
#embed __FILE__ if_empty (42; int c = 0) limit (0)
;
const unsigned char magna_carta[] = {
#embed <magna-carta.txt> prefix (/* This is a prefix */) suffix (, 0) if_empty (0)
};
const unsigned char d[] = {
#define MAGNA_CARTA "magna-carta.txt"
#define MAGNA_CARTA_PARAMS prefix (239, 187, 191,) \
suffix (, 0) limit (15)
#embed MAGNA_CARTA MAGNA_CARTA_PARAMS \
\
if_empty (0{1[2[3{4(5(6)7)8}9]0]1}2)
};
#ifndef __cplusplus
const unsigned char e[] = {
#embed MAGNA_CARTA __limit__ (42) __suffix__ (, [21] = 'X', [22] = 'X', [23] = 'X')
};
#endif
int f;
struct S { int a; long b; unsigned short c; long long d; } g = {
#embed "magna-carta.txt" limit (4)
};
const unsigned char h[] = {
#embed __FILE__ suffix (, 0)
};
struct T { int a; };
struct U { unsigned short a; struct T b; long long c; };
struct V { unsigned long long a; struct U b; short int c; int d; };
struct V v = {
#embed "magna-carta.txt" __limit__(4)__prefix__(42,)__suffix__(,-42)
};
#ifndef __cplusplus
const unsigned char w[] = {
#embed <magna-carta.txt> prefix([0] = 42, [72] =) limit(1500)
};
#endif
const unsigned char x[] = {
#define XIFEMPTY1 0, 1
#define XIFEMPTY2 ,3,
#define XIFEMPTY3 4
#embed "../empty.h" if_empty (XIFEMPTY1,2 XIFEMPTY2 \
XIFEMPTY3) prefix (5, 6, 7, ) suffix (, 8, 9)
};
const unsigned char y[] = {
#embed <embed-3.c> suffix(,0)
};
const unsigned char z[] = {
#embed "embed-3.c" suffix(,0)
};

#if 0
#embed <non-existent-file.bin> limit (3)
#endif

#ifdef __cplusplus
#define C "C"
#else
#define C
#endif
extern C void abort (void);
#ifdef __cplusplus
extern C const char *strstr (const char *, const char *);
#else
extern C char *strstr (const char *, const char *);
#endif

void
foo (unsigned int a, unsigned long long b, unsigned char c, int d, int e)
{
  if (a != 'H' || b != 'e' || c != 'n' || d != 'r' || !e)
    abort ();
}

int
main ()
{
  if (a != '/' || b != 42 || c != 0)
    abort ();
  if (sizeof (magna_carta) != 17893 + 1
      /* Allow for DOS line endings instead of Unix just in case.  */
      && sizeof (magna_carta) != 17939 + 1)
    abort ();
  if (!strstr ((const char *) magna_carta, "imprisonétur"))
    abort ();
  if (sizeof (d) != 3 + 15 + 1
      || d[0] != 239 || d[1] != 187 || d[2] != 191 || d[3 + 15] != 0)
    abort ();
#ifndef __cplusplus
  if (sizeof (e) != 42 || e[21] != 'X' || e[22] != 'X' || e[23] != 'X')
    abort ();
#endif
  f = (
#embed "magna-carta.txt" limit (4) prefix (172 + ) suffix (+ 2)
  );
  if (f != 'r' + 2)
    abort ();
  if (g.a != 'H' || g.b != 'e' || g.c != 'n' || g.d != 'r')
    abort ();
  if (!strstr ((const char *) h, "imprisonétur")
      || !strstr ((const char *) h, "blah blah"))
    abort ();
  if (v.a != 42 || v.b.a != 'H' || v.b.b.a != 'e'
      || v.b.c != 'n' || v.c != 'r' || v.d != -42)
    abort ();
#ifndef __cplusplus
  if (sizeof (w) != 1572 || w[0] != 42 || w[1] != 0 || w[71] != 0
      || w[72] != 'H' || w[73] != 'e' || w[74] != 'n' || w[75] != 'r')
    abort ();
#endif
  if (sizeof (x) != 5 || x[0] != 0 || x[1] != 1 || x[2] != 2
      || x[3] != 3 || x[4] != 4)
    abort ();
  foo (
#embed <magna-carta.txt> limit(5)
  );
  if (sizeof (y) > 100
      || !strstr ((const char *) y, "Dummy file.")
      || strstr ((const char *) y, "__gnu__::__non_existent_parameter"))
    abort ();
  if (sizeof (z) < 100
      || strstr ((const char *) z, "Dummy file.")
      || !strstr ((const char *) z, "__gnu__::__non_existent_parameter"))
    abort ();
  if (sizeof (
#embed "embed-1.inc"
      ) != sizeof (int))
    abort ();
#ifndef __cplusplus
  if (_Generic (
#embed "embed-1.inc" __limit__ (1) __suffix__ (, int: 42, default: 0)
      ) != 42)
    abort ();
#endif
#if __STDC_VERSION__ >= 202311L
  typeof (
#embed "embed-1.inc" limit (1)
  ) j;
  if (sizeof (j) != sizeof (int))
    abort ();
#elif __cplusplus >= 201103L
  decltype (
#embed "embed-1.inc" limit (1)
  ) j;
  if (sizeof (j) != sizeof (int))
    abort ();
#endif
#if __has_embed ("embed-5.c" ds9000::element_type(short))
  short meow[] = {
    #embed "embed-5.c" ds9000::element_type(short)
  };
#elif __has_embed ("embed-5.c")
  const unsigned char meow_bytes[] = {
    #embed "embed-5.c"
  };
  short meow[sizeof (meow_bytes) / sizeof (short)] = {};
  for (int i = 0; i < (int) (sizeof (meow) / sizeof (short)); i++)
    meow[i] = (meow_bytes[i * 2] << 8) | meow_bytes[i * 2 + 1];
#else
  #error "cannot find embed-5.c resource"
#endif
  if (meow[0] != ('/' << 8) + '*')
    abort ();
#if __has_embed("././././embed-1.c" __limit__(1))
  if (
		#embed		"././././embed-1.c" /* This */ limit /* is */ ( /* a */ 1 /*comment*/) // .
      != '/')
    abort ();
#else
#error "__has_embed fail"
#endif
}
