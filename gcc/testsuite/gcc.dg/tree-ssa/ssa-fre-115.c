/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

extern void bar(void);
extern void foo1(void);

void foo (int cond1, int x, int a, int b)
{
  if (a == b)
    {
      if (a != b)
        goto dead;
        
      if (!cond1) return;
        
      switch (x) {
        case 1: case 10: case 20: case 30: case 40:
        case 50: case 60: case 70: case 80: case 90:
          goto dead;
        case 11:
          foo1();
          goto dead;
        default:
          return;
      }
      
    dead:
      if (!cond1)
        bar();
    }
}

/* { dg-final { scan-tree-dump-not "bar" "fre1" } } */
