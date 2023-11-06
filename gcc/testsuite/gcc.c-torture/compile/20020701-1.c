/* PR target/7177
   Problem with cris-axis-elf: ICE in global.
   Origin: hp@axis.com.  */

typedef __SIZE_TYPE__ size_t;
void f1 (void *);
char *f2 (const char *);
int atoi (const char *);
char *strchr (const char *, int);
int strcmp (const char *, const char *);
size_t strlen (const char *);
typedef enum { A, B, C } t1;
extern const char _v[];

static t1
f (const char* p1, const char* p2, char p3)
{
  char *v1;
  char *v2;
  char *a;
  char *v3;
  char *v4;
  char *v5;
  char *e;
  char *v6;
  t1 r = C;

  v1 = f2 (p2);
  v4 = f2 (p1);

  a = v2 = v1;
  e = v5 = v4;
  __builtin_memcpy (&e, &e, sizeof (e));

  v3 = strchr (v2, ',');
  v6 = strchr (v5, ',');

  while ((_v + 1)[(unsigned) *a] & 4)
    a++;
  while ((_v + 1)[(unsigned) *e] & 4)
    e++;

  if (a == v3 && e == v6)
    {
      if (p3)
        r = atoi (v5) < atoi (v2) ? B : A;
      else
        r = atoi (v5) > atoi (v2) ? B : A;
      v2 = ++a;
      v5 = ++e;
      v3 = strchr (v2, ',');
      v6 = strchr (v5, ',');

      while ((_v + 1)[(unsigned) *a] & 4)
        a++;
      while ((_v + 1)[(unsigned) *e] & 4)
        e++;

      if (a == v3 && e == v6)
        {
          if (r == B)
            r = B;
          else if (p3)
            r = atoi (v5) < atoi (v2) ? B : A;
          else
            r = atoi (v5) > atoi (v2) ? B : A;
        }
      else
        r = C;
    }

  f1 (v1);
  f1 (v4);
  return r;
}
