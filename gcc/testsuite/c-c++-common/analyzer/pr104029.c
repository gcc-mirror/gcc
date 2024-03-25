typedef __SIZE_TYPE__ size_t;
typedef const void *t_comptype;
typedef int (*t_compfunc)(t_comptype, t_comptype);

extern int *__errno_location(void)
  __attribute__((__nothrow__, __leaf__,__const__));
extern void free(void *__ptr)
  __attribute__((__nothrow__, __leaf__));
extern void *my_malloc1(const char *file, int line, size_t size);

int heapsort(void *vbase, size_t nmemb, size_t size, t_compfunc compar) {
  char tmp, *tmp1, *tmp2, *abase, *k, *p, *t;
  size_t cnt, i, j, l;

  if (nmemb <= 1)
    return (0);

  if (!size) {
    (*__errno_location()) = 22;
    return (-1);
  }

  k = (char *) my_malloc1(__FILE__, __LINE__, size);

  abase = (char *)vbase - size;

  for (l = nmemb / 2 + 1; --l;) {
    for (i = l; (j = i * 2) <= nmemb; i = j) {
      p = abase + j * size;
      if (j < nmemb && compar(p, p + size) < 0) {
        p += size;
        ++j;
      }
      t = abase + i * size;
      if (compar(p, t) <= 0)
        break;
      {
        cnt = size;
        do {
          tmp = *t;
          *t++ = *p;
          *p++ = tmp;
        } while (--cnt);
      };
    }
  };

  while (nmemb > 1) {
    {
      cnt = size;
      tmp1 = k;
      tmp2 = abase + nmemb * size;
      do {
        *tmp1++ = *tmp2++;
      } while (--cnt);
    };
    {
      cnt = size;
      tmp1 = abase + nmemb * size;
      tmp2 = abase + size;
      do {
        *tmp1++ = *tmp2++;
      } while (--cnt);
    };
    --nmemb;
    {
      for (i = 1; (j = i * 2) <= nmemb; i = j) {
        p = abase + j * size;
        if (j < nmemb && compar(p, p + size) < 0) {
          p += size;
          ++j;
        }
        t = abase + i * size;
        {
          cnt = size;
          tmp1 = t;
          tmp2 = p;
          do {
            *tmp1++ = *tmp2++;
          } while (--cnt);
        };
      }
      for (;;) {
        j = i;
        i = j / 2;
        p = abase + j * size;
        t = abase + i * size;
        if (j == 1 || compar(k, t) < 0) {
          {
            cnt = size;
            tmp1 = p;
            tmp2 = k;
            do {
              *tmp1++ = *tmp2++;
            } while (--cnt);
          };
          break;
        }
        {
          cnt = size;
          tmp1 = p;
          tmp2 = t;
          do {
            *tmp1++ = *tmp2++;
          } while (--cnt);
        };
      }
    };
  }
  free(k);
  return (0);
}
