/* { dg-do compile } */
/* { dg-options "-O2 -fipa-sra -fdump-tree-eipa_sra-details"  } */

extern void consume (int);
extern int glob, glob1, glob2;
extern int get (void);


static void __attribute__ ((noinline))
foo (int a)
{
  a = glob;
  consume (a);
  a = get ();
  consume (a);
  __asm__ volatile("" : : ""(a));
  consume (a);

  if (glob1)
    a = glob1;
  else
    a = glob2;
  consume (a);
}

int
bar (int a)
{
  foo (a);
  glob = a;
  return 0;
}

/* { dg-final { scan-tree-dump-times "replacing an SSA name of a removed param" 4 "eipa_sra" } } */
