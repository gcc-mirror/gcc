/* PR target/49163 */
/* { dg-require-effective-target int32plus } */
struct S1
{
 unsigned f0:18;
 int f1;
} __attribute__ ((packed));

struct S2
{
  volatile long long f0;
  int f1;
};

struct S1 s1;
struct S2 s2;
const struct S2 s2array[2][1] = { };

struct S2 **sptr;

extern int bar (char a, long long b, int * c, long long d, long long e);
extern int baz (void);

int i;
int *ptr;

void
foo (int *arg)
{
  for (i = 0; i < 1; i = baz())
    {
      *arg = *(int *)sptr;
      *ptr = bar (*arg, s2.f1, ptr,
		  bar (s2array[1][0].f0, *arg, ptr, s1.f1, *ptr), *arg);
    }
}
