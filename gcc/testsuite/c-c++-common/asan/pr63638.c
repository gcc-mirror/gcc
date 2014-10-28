/* { dg-do compile } */

extern
#ifdef __cplusplus
"C"
#endif
void *memcpy (void *, const void *, __SIZE_TYPE__);

struct S{
  long d0, d1, d2, d3, d4, d5, d6;
};

struct S s[6];

int f(struct S *p)
{
  memcpy(p, &s[2], sizeof(*p));
  memcpy(p, &s[1], sizeof(*p));
}

