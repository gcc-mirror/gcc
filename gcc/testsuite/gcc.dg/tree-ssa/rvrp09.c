/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-rvrp-details -fdisable-tree-ethread -fdisable-tree-forwprop1 -fdisable-tree-ccp1  -fdisable-tree-fre1 " } */

extern void exit (int);
extern void abort (void);


void
signed_foo (char x)
{

  char a = x & 0x00;    
  if (a > 0)   /*  This should fold  */
    abort();

  char b = x & 0x0f;
  if (b > 0x0f)
    abort ();

  char c = x & 0x3c;
  if (c > 0x3c)
    abort ();

  char d = x & 0xf0;
  if (d > 0)
     if (d < 16)
       abort();
}

/* { dg-final { scan-tree-dump-times "Branch rewritten" 4 "rvrp"} } */
