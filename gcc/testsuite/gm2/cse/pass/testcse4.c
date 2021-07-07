void *alloca (unsigned int);
void memcpy(void *, void *, unsigned int);
void write (int, void *, int);

#define nul  (char)0
struct m2string {
  char *contents;
  int HIGH;
};

struct m2string at_start = { "hello", 5 };

static int StrLen (struct m2string a)
{
  int high, len;
  char *copy;  
  char **T25;

  copy = alloca(a.HIGH+1);
  memcpy(a.contents, copy, a.HIGH+1);
  a.contents = copy;
#if 0
  len = 0;
  high = a.HIGH;
#endif
  
  T25 = (char **)&a;
#if 0
  /* (a.contents[len] != nul) */
  while ((len <= high) && ((*T24)[len] != nul))
    len++;

  return len;
#endif
  (*T25)[0] = 'a';
  return 0;
}

void init (void)
{
  struct m2string b;

  b.contents = "hello";
  b.HIGH = 4;
  if (StrLen(b) == 5)
    write(1, "works\n", 6);
}

