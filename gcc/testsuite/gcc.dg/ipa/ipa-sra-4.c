/* { dg-do compile } */
/* { dg-options "-O2 -fipa-sra -fno-ipa-pure-const -fdump-ipa-sra -fno-ipa-modref" } */

static int
__attribute__((noinline))
ox (int *i)
{
  return *i+4**i;
}

int *holder;

static int
__attribute__((noinline))
ox_ctrl_1 (int *j)
{
  holder = j;
  return *j+4 * *j+1;
}

static void
__attribute__((noinline))
ox_ctrl_2 (int *k)
{
  *k = 8;
}

static int zzz[10];

static int
__attribute__((noinline))
ox_improved (int recurse, int *l)
{
  int r = 0;

  r = *l;

  if (recurse)
    {
      if (recurse > 2)
	l = &zzz[3];
      else
	l = zzz;

      ox_improved (0, l);
    }

  return r;
}

void caller (void)
{
  int a = 1;
  int b = 10;
  int c;

  c = ox (&a);
  ox_ctrl_1 (&a);
  ox_ctrl_2 (&a);
  *holder = ox_improved (1, &b) + c;
  return;
}

/* { dg-final { scan-ipa-dump-times "Will split parameter" 2 "sra"  } } */

