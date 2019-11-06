/* PR middle-end/92373 - ICE in -Warray-bounds on access to member array
   in an initialized char buffer
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

void sink (void*);

struct S
{
  char data[1];
};

char a[6] = { };

int f (void)
{
  struct S *p = (struct S*)a;
  return p->data[4];

}

void g (void)
{
  char b[6] = { };
  struct S *p = (struct S*)b;
  p->data[4] = 0;
  sink (p);
}
