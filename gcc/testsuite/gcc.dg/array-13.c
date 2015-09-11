/* { dg-do run } */
/* { dg-options "" } */

/* Verify that GCC's initialized flexible array member extension
   works properly.  */

extern void abort(void);
extern void exit(int);

typedef int T[];
typedef int T0[0];

struct f { int w; T x; };
struct g { int w; T0 x; };

static struct f f = { 4, { 0, 1, 2, 3 } };
static int junk1[] = { -1, -1, -1, -1 };
static struct g g = { 4, { 0, 1, 2, 3 } }; /* { dg-warning "(excess elements)|(near initialization)" "" } */
static int junk2[] = { -1, -1, -1, -1 };

int main()
{
  int i;
  for (i = 0; i < f.w; ++i)
    if (f.x[i] != i)
      abort ();
  exit(0);
}
