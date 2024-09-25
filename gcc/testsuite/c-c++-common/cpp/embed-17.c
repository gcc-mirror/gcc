/* { dg-do run } */
/* { dg-options "--embed-dir=${srcdir}/c-c++-common/cpp/embed-dir" } */
/* { dg-additional-options "-std=gnu99" { target c } } */

#if __has_embed ("." gnu::base64 ("")) != __STDC_EMBED_EMPTY__
#error "__has_embed fail"
#endif

#if __has_embed ("." gnu::base64 ("SA==")) != __STDC_EMBED_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed ("." prefix(-) suffix (-) if_empty (-) __gnu__::__base64__ ("SA==")) != __STDC_EMBED_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed ("." gnu::__base64__ ("SGU=")) != __STDC_EMBED_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed ("." gnu::__base64__ ("SGVs")) != __STDC_EMBED_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed ("." __gnu__::base64 ("SGVsbG8=")) != __STDC_EMBED_FOUND__
#error "__has_embed fail"
#endif

/* M. Tulli Ciceronis De Finibus Bonorum et Malorum.  Liber Primus.  */
/* echo "Tm9u....bnQu" | fmt -s -w 76 | base64 -d to decode.  */
#define BONORUM_ET_MALORUM \
"Tm9uIGVyYW0gbsOpc2NpdXMsIEJydXRlLCBjdW0sIHF1w6Ygc3VtbWlzIGluZ8OpbmlpcyBleHF1aXNpdMOhcXVlIGRvY3Ryw61uYSBwaGlsw7Nzb3BoaSBHcsOmY28gc2VybcOzbmUgdHJhY3RhdsOtc3NlbnQsIGVhIExhdMOtbmlzIGzDrXR0ZXJpcyBtYW5kYXLDqW11cywgZm9yZSB1dCBoaWMgbm9zdGVyIGxhYm9yIGluIHbDoXJpYXMgcmVwcmVoZW5zacOzbmVzIGluY8O6cnJlcmV0LiBuYW0gcXVpYsO6c2RhbSwgZXQgaWlzIHF1aWRlbSBub24gw6FkbW9kdW0gaW5kw7NjdGlzLCB0b3R1bSBob2MgZMOtc3BsaWNldCBwaGlsb3NvcGjDoXJpLiBxdWlkYW0gYXV0ZW0gbm9uIHRhbSBpZCByZXByZWjDqW5kdW50LCBzaSByZW3DrXNzaXVzIGFnw6F0dXIsIHNlZCB0YW50dW0gc3TDumRpdW0gdGFtcXVlIG11bHRhbSDDs3BlcmFtIHBvbsOpbmRhbSBpbiBlbyBub24gYXJiaXRyw6FudHVyLiBlcnVudCDDqXRpYW0sIGV0IGlpIHF1aWRlbSBlcnVkw610aSBHcsOmY2lzIGzDrXR0ZXJpcywgY29udGVtbsOpbnRlcyBMYXTDrW5hcywgcXVpIHNlIGRpY2FudCBpbiBHcsOmY2lzIGxlZ8OpbmRpcyDDs3BlcmFtIG1hbGxlIGNvbnPDum1lcmUuIHBvc3Ryw6ltbyDDoWxpcXVvcyBmdXTDunJvcyBzw7pzcGljb3IsIHF1aSBtZSBhZCDDoWxpYXMgbMOtdHRlcmFzIHZvY2VudCwgZ2VudXMgaG9jIHNjcmliw6luZGksIGV0c2kgc2l0IGVsw6lnYW5zLCBwZXJzw7Nuw6YgdGFtZW4gZXQgZGlnbml0w6F0aXMgZXNzZSBuZWdlbnQu"
#if __has_embed ("." gnu::base64 (BONORUM_ET_MALORUM)) != __STDC_EMBED_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed ("foo" gnu::base64 ("SGU=")) != __STDC_EMBED_NOT_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed (<foo> gnu::base64 ("SGU=")) != __STDC_EMBED_NOT_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed (<.> gnu::base64 ("SGU=")) != __STDC_EMBED_NOT_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed ("." gnu::base64 ("SGU=") limit(5)) != __STDC_EMBED_NOT_FOUND__
#error "__has_embed fail"
#endif

#if __has_embed ("." gnu::base64 ("SGU=") gnu::offset(2)) != __STDC_EMBED_NOT_FOUND__
#error "__has_embed fail"
#endif

#embed "." gnu::base64 ("") if_empty (int a = 42;) prefix(+ +) suffix (+ +)
#embed "." __gnu__::__base64__ ("SA==") prefix (int b = ) suffix (;) if_empty (+ +)
const unsigned char c[] = {
  #embed "." gnu::base64("SGU=")
};
const unsigned char d[] = {
  #embed "." gnu::base64 ("SGVs")
};
const unsigned char e[] = {
  #embed "." gnu::base64 ("SGVsbG8=")
};
const unsigned char f[] = {
#ifdef __cplusplus
  #embed "." gnu::base64 (BONORUM_ET_MALORUM) prefix (' ', )
#else
  #embed "." gnu::base64 (BONORUM_ET_MALORUM) prefix ([1] = ) suffix(, [0] = ' ')
#endif
};
#if __has_embed ("." gnu::base64("TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdCwg" \
"c2VkIGRvIGVpdXNtb2QgdGVtcG9yIGluY2lkaWR1bnQgdXQgbGFib3JlIGV0IGRvbG9yZSBtYWdu" \
"YSBhbGlxdWEuCg==")) == __STDC_EMBED_FOUND__
const unsigned char g[] = {
#embed "." gnu::base64("" \
"T" "G9" "yZW" \
"0gaX" \
"BzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdCwg" \
"c2VkIGRvIGVpdXNtb2QgdGVtcG9yIGluY2lkaWR1bnQgdXQgbGFib3JlIGV0IGRvbG9yZSBtYWdu" \
"YSBhbGlxdWEuCg==")
};
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
  if (a != 42 || b != 'H')
    abort ();
  if (sizeof (c) != 2 || c[0] != 'H' || c[1] != 'e')
    abort ();
  if (sizeof (d) != 3 || d[0] != 'H' || d[1] != 'e' || d[2] != 'l')
    abort ();
  if (sizeof (e) != 5 || memcmp (e, "Hello", 5))
    abort ();
  if (sizeof (f) != 1 + 747 || memcmp (f, " Non eram néscius, Brute",
				       sizeof (" Non eram néscius, Brute") - 1))
    abort ();
  const char ge[]
    = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.";
  if (sizeof (g) != sizeof (ge)
      || memcmp (g, ge, sizeof (ge) - 1)
      || g[sizeof (ge) - 1] != '\n')
    abort ();
}
