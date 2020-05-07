/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

typedef struct
{
  int p1;
  int p2;
  int p3;
} P;
struct S
{
  int field;
};
extern int v2;
extern void foo (struct S *map);
static struct S var;
const P *pv;
int ps;
void
f (void)
{
  if (pv != 0)
    for (const P *ph = pv; ph < &pv[ps]; ++ph)
      switch (ph->p1)
	{
	case 1:
	v2 = ph->p2;
	break;
	case 2:
	var.field = ph->p3;
	break;
	}
  if (var.field != 0) /* { dg-bogus "uninitialized" } */
    foo (&var);
}
