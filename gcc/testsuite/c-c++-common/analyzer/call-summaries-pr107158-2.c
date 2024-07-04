/* { dg-additional-options "-fanalyzer-call-summaries -Wno-analyzer-too-complex -Wno-analyzer-symbol-too-complex" } */
/* { dg-skip-if "c++98 has no noreturn attribute" { c++98_only } } */

#ifdef __cplusplus
#define _Noreturn [[noreturn]]
#endif

typedef __SIZE_TYPE__ size_t;
typedef struct _IO_FILE FILE;
extern char *fgets(char *__restrict __s, int __n, FILE *__restrict __stream)
    __attribute__((__access__(__write_only__, 1, 2)));
extern void perror(const char *__s);
enum {
  _ISspace = ((5) < 8 ? ((1 << (5)) << 8) : ((1 << (5)) >> 8)),
};
extern const unsigned short int **__ctype_b_loc(void)
    __attribute__((__nothrow__, __leaf__)) __attribute__((__const__));
extern void *malloc(size_t __size) __attribute__((__nothrow__, __leaf__))
__attribute__((__malloc__)) __attribute__((__alloc_size__(1)));
extern void exit(int __status) __attribute__((__nothrow__, __leaf__))
__attribute__((__noreturn__));
extern char *strcpy(char *__restrict __dest, const char *__restrict __src)
    __attribute__((__nothrow__, __leaf__)) __attribute__((__nonnull__(1, 2)));
extern size_t strlen(const char *__s) __attribute__((__nothrow__, __leaf__))
__attribute__((__pure__)) __attribute__((__nonnull__(1)));

struct mydata {
  struct mydata *link;
  char *name;
  char *type;
};

static struct mydata *all_data;
static int line_no;

_Noreturn static void failed(const char *message) {
  perror(message);
  exit(1);
}

static char *string_dup(const char *string) {
  char *buf;

  if ((buf = (char *) malloc(strlen(string) + 1)) == ((void *)0))
    failed("malloc() failed");

  return strcpy(buf, string);
}

static void store_data(const char *name, const char *type) {
  struct mydata *p, *q;

  if ((p = (struct mydata *)malloc(sizeof(struct mydata))) == ((void *)0))
    failed("malloc() failed");

  p->link = (struct mydata *)((void *)0);
  p->name = string_dup(name);
  p->type = string_dup(type);

  if ((q = all_data) == ((void *)0))
    all_data = p;
  else {
    while (q->link != ((void *)0))
      q = q->link;
    q->link = p;
  }
}

static void parse_tbl(char *buffer) {
  char *s = buffer;
  char *t = s + strlen(s);

  do {
    t--;
    if (((*__ctype_b_loc())[(int)(((int)*t))] & (unsigned short int)_ISspace))
      *t = '\0';
    else
      break;
  } while (t > s);
  while (((*__ctype_b_loc())[(int)(((int)*s))] & (unsigned short int)_ISspace))
    s++;
  buffer = s;

  line_no++;
  if (*buffer != ';' && *buffer != '\0') {
    if (*buffer == '#') {
      store_data(buffer, ""); /* { dg-bogus "leak" "PR analyzer/107158" { xfail *-*-* } } */
    } else {

      while (*s && !((*__ctype_b_loc())[(int)(((int)*s))] &
                     (unsigned short int)_ISspace))
        s++;
      while (
          ((*__ctype_b_loc())[(int)(((int)*s))] & (unsigned short int)_ISspace))
        *s++ = '\0';
      store_data(buffer, s); /* { dg-bogus "leak" "PR analyzer/107158" { xfail *-*-* } } */
    }
  }
}

/* [...snip...] */

static void makecfg(FILE *ifp, FILE *ofp, FILE *ofp2) {
  char buffer[8192];

  /* [...snip...] */

  line_no = 0;
  while (fgets(buffer, sizeof(buffer) - 1, ifp))
    parse_tbl(buffer);

  /* [...snip...] */
}
