/* { dg-require-effective-target int32plus } */
/* { dg-additional-options "-fanalyzer-call-summaries --param analyzer-min-snodes-for-call-summary=0 -Wno-analyzer-symbol-too-complex" } */

/* There need to be at least two calls to a function for the
   call-summarization code to be used.
   TODO: add some kind of test that summarization *was* used.  */

/* Reduced from an example in Emacs in which string_char_and_length
   was being incorrectly summarized, failing to see the write to *length.  */

typedef long int ptrdiff_t;
typedef struct Lisp_X *Lisp_Word;
typedef Lisp_Word Lisp_Object;
extern _Bool STRING_MULTIBYTE(Lisp_Object str);
extern unsigned char *SDATA(Lisp_Object string);
enum { MAX_2_BYTE_CHAR = 0x7FF };
enum { MAX_3_BYTE_CHAR = 0xFFFF };
enum { MAX_4_BYTE_CHAR = 0x1FFFFF };
enum { MAX_5_BYTE_CHAR = 0x3FFF7F };
extern int make_char_multibyte(int c);
static inline int string_char_and_length(unsigned char const *p, int *length) {
  int c = p[0];
  if (!(c & 0x80)) {
    *length = 1;
    return c;
  }
  ((0xC0 <= c) ? (void)0 : __builtin_unreachable());
  int d = (c << 6) + p[1] - ((0xC0 << 6) + 0x80);
  if (!(c & 0x20)) {
    *length = 2;
    return d + (c < 0xC2 ? 0x3FFF80 : 0);
  }
  d = (d << 6) + p[2] - ((0x20 << 12) + 0x80);
  if (!(c & 0x10)) {
    *length = 3;
    ((MAX_2_BYTE_CHAR < d && d <= MAX_3_BYTE_CHAR)
     ? (void)0
     : __builtin_unreachable());
    return d;
  }
  d = (d << 6) + p[3] - ((0x10 << 18) + 0x80);
  if (!(c & 0x08)) {
    *length = 4;
    ((MAX_3_BYTE_CHAR < d && d <= MAX_4_BYTE_CHAR)
     ? (void)0
     : __builtin_unreachable());
    return d;
  }
  d = (d << 6) + p[4] - ((0x08 << 24) + 0x80);
  *length = 5;
  ((MAX_4_BYTE_CHAR < d && d <= MAX_5_BYTE_CHAR)
   ? (void)0
   : __builtin_unreachable());
  return d;
}
int fetch_string_char_advance(Lisp_Object string,
			      ptrdiff_t *charidx,
			      ptrdiff_t *byteidx) {
  int output;
  ptrdiff_t b = *byteidx;
  unsigned char *chp = SDATA(string) + b;
  if (STRING_MULTIBYTE(string)) {
    int chlen;
    output = string_char_and_length(chp, &chlen);
    b += chlen;
  } else {
    output = *chp;
    b++;
  }
  (*charidx)++;
  *byteidx = b;
  return output;
}
int fetch_string_char_as_multibyte_advance(Lisp_Object string,
					   ptrdiff_t *charidx,
					   ptrdiff_t *byteidx) {
  int output;
  ptrdiff_t b = *byteidx;
  unsigned char *chp = SDATA(string) + b;
  if (STRING_MULTIBYTE(string)) {
    int chlen;
    output = string_char_and_length(chp, &chlen);
    b += chlen;
  } else {
    output = make_char_multibyte(*chp);
    b++;
  }
  (*charidx)++;
  *byteidx = b;
  return output;
}
