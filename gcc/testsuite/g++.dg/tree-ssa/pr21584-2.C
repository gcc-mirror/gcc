extern char *strcpy (char *__restrict __dest, __const char *__restrict __src);

extern char *foo (void);
extern void *malloc(__SIZE_TYPE__) __attribute__((malloc));

char v[100];

void baz()
{
  char *vec;
  char buf[512];

  char *p = buf;
  while (v[(*p)])
    p++;

  if (*p != '#' && (p = foo()) != 0) {
    strcpy ((char*)malloc(10), p);
  }
}


