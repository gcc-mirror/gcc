/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom2" } */
int link_error(void);
int s(void);

int t(int i)
{
  _Bool g = i == 4;
 int h = g;
 _Bool j = h;
 int k = j;
 _Bool l = k == 0;
 _Bool o = !l;
 int m = o;

 if (m)
  if (i != 4)
   return link_error();
 return 0;
}

/* There should be no link_error calls, if there is any, the
   optimization has failed */
/* { dg-final { scan-tree-dump-times "link_error" 0 "dom2"} } */
/* { dg-final { cleanup-tree-dump "dom2" } } */
