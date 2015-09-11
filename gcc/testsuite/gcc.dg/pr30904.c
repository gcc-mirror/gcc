/* { dg-do link } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int t;
extern void link_error(void);
int main (void)
{
      struct { unsigned int a : 7; } s;
      s.a = t;
      if (s.a >> 8)
          link_error ();
      if (s.a >> 9)
          link_error ();
}


/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized"  } } */
