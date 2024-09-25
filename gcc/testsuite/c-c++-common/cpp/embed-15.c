/* { dg-do run } */
/* { dg-options "--embed-dir=${srcdir}/c-c++-common/cpp/embed-dir" } */
/* { dg-additional-options "-std=gnu99" { target c } } */

#if __has_embed (__FILE__ gnu::offset (4 + FOOBAR) limit (3)) != __STDC_EMBED_FOUND__
#error "__has_embed fail"
#endif

#embed <magna-carta.txt> limit(1) gnu::offset (0) prefix(int a = ) suffix (;) 
#embed <magna-carta.txt> limit(1) __gnu__::offset (1 * 1) prefix(int b = ) suffix (;) 
#embed <magna-carta.txt> limit(1) gnu::__offset__ (1 + 1) prefix(int c = ) suffix (;) 
#embed <magna-carta.txt> __limit__(1) __gnu__::__offset__ (1 + (1 \
  + 1)) __prefix__(int d = ) __suffix__ (;)
const unsigned char e[] = {
  #embed <magna-carta.txt> limit(5) gnu::offset (999)
};
const unsigned char f[] = {
  #embed <magna-carta.txt> limit(7) gnu::offset (998)
};
const unsigned char g[] = {
  #embed <magna-carta.txt> limit(8) gnu::offset (998)
};
const unsigned char h[] = {
  #embed <magna-carta.txt> limit(8) gnu::offset (997)
};
const unsigned char i[] = {
  #embed <magna-carta.txt> limit(9) gnu::offset (997)
};
const unsigned char j[] = {
  #embed <magna-carta.txt> limit(30) gnu::offset (990)
};
const unsigned char k[] = {
  #embed <magna-carta.txt> limit(26) gnu::offset (992)
};
const unsigned char l[] = {
  #embed <magna-carta.txt>
};
const unsigned char m[] = {
  #embed <magna-carta.txt> __limit__ (1000) __gnu__::__offset__ (32)
};
#if __has_embed (<magna-carta.txt> limit(5) gnu::offset (999)) != __STDC_EMBED_FOUND__ \
    || __has_embed (<magna-carta.txt> limit(5) gnu::offset (999)) != __STDC_EMBED_FOUND__ \
    || __has_embed (<magna-carta.txt> limit(7) gnu::offset (998)) != __STDC_EMBED_FOUND__ \
    || __has_embed (<magna-carta.txt> limit(8) gnu::offset (998)) != __STDC_EMBED_FOUND__ \
    || __has_embed (<magna-carta.txt> limit(8) gnu::offset (997)) != __STDC_EMBED_FOUND__ \
    || __has_embed (<magna-carta.txt> limit(9) gnu::offset (997)) != __STDC_EMBED_FOUND__ \
    || __has_embed (<magna-carta.txt> limit(30) gnu::offset (990)) != __STDC_EMBED_FOUND__ \
    || __has_embed (<magna-carta.txt> limit(26) gnu::offset (992)) != __STDC_EMBED_FOUND__ \
    || __has_embed (<magna-carta.txt>) != __STDC_EMBED_FOUND__ \
    || __has_embed (<magna-carta.txt> limit(26) gnu::offset (992)) != __STDC_EMBED_FOUND__
#error "__has_embed fail"
#endif

#ifdef __cplusplus
#define C "C"
#else
#define C
#endif
extern C void abort (void);
extern C int memcmp (const void *, const void *, __SIZE_TYPE__);

int
main ()
{
  if (a != 'H' || b != 'e' || c != 'n' || d != 'r')
    abort ();
  if (sizeof (e) != 5
      || sizeof (f) != 7
      || sizeof (g) != 8
      || sizeof (h) != 8
      || sizeof (i) != 9
      || sizeof (j) != 30
      || sizeof (k) != 26
      || sizeof (l) < 1032
      || sizeof (m) != 1000)
    abort ();
  if (memcmp (e, l + 999, 5)
      || memcmp (f, l + 998, 7)
      || memcmp (g, l + 998, 8)
      || memcmp (h, l + 997, 8)
      || memcmp (i, l + 997, 9)
      || memcmp (j, l + 990, 30)
      || memcmp (k, l + 992, 26)
      || memcmp (m, l + 32, 1000))
    abort ();
  if (l[0] != 'H' || l[1] != 'e' || l[2] != 'n' || l[3] != 'r')
    abort ();
}
