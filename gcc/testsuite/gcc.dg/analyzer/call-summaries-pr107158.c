/* { dg-additional-options "-fanalyzer-call-summaries" } */

typedef __SIZE_TYPE__ size_t;
enum { _ISspace = ((5) < 8 ? ((1 << (5)) << 8) : ((1 << (5)) >> 8)) };
extern const unsigned short int **__ctype_b_loc(void)
  __attribute__((__nothrow__, __leaf__, __const__));
extern void *malloc(size_t __size)
  __attribute__((__nothrow__, __leaf__, __malloc__, __alloc_size__(1)));
extern char *strcpy(char *__restrict __dest, const char *__restrict __src)
  __attribute__((__nothrow__, __leaf__, __nonnull__(1, 2)));
extern size_t strlen(const char *__s)
  __attribute__((__nothrow__, __leaf__, __pure__, __nonnull__(1)));

struct mydata {
  struct mydata *link;
  char *name;
  char *type;
};

static struct mydata *all_data;
static int line_no;

__attribute__((__noreturn__)) void failed(const char *message);

static char *string_dup(const char *string) {
  char *buf;

  if ((buf = malloc(strlen(string) + 1)) == ((void *)0))
    failed("malloc() failed");

  return strcpy(buf, string);
}

static void store_data(const char *name, const char *type) {
  struct mydata *p, *q;

  if ((p = (struct mydata *)malloc(sizeof(struct mydata))) == ((void *)0))
    failed("malloc() failed");

  p->link = ((void *)0);
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
