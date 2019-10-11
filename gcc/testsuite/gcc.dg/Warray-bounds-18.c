/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds -Wno-stringop-overflow" } */

typedef struct
{
  int len;
  char data[1];
} rec;

int
p(rec *r, int len);

int
f (char prm1, char prm2)
{
  char buf[10];

  rec *r1 = (rec *)&buf;

  r1->len = 10;
  r1->data[0] = prm1;
  r1->data[1] = prm2; /* { dg-bogus "above array bounds" } */

  return p(r1, r1->len);
}
