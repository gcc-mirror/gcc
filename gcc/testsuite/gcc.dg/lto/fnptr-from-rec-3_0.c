/* { dg-lto-do link } */
/* { dg-lto-options { { -O3 -flto -fdump-ipa-devirt } } } */


struct fnptrs
{
  void (*first)(void);
  int (*dostuff)(int);
};

int noop (int n);

struct fnptrs fp = {(void (*)(void)) 0, noop};


void __attribute__ ((noinline)) init (void);

volatile int v;

int __attribute__ ((noipa))
get_count (void)
{
  return 4;
};


void __attribute__ ((noinline))
loop (void)
{
  for (int i = 0; i < get_count (); i++)
    {
      v = v + fp.dostuff (v);
    }
}

int main (int argc, char **argv)
{
  init ();
  loop ();
  return 0;
}

/* { dg-final { scan-wpa-ipa-dump "num speculative call targets: 1"  "devirt"  } } */
