/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-reassoc2" } */

/* Slightly changed testcase from PR middle-end/40815.  */
void bar(char*, char*, int);
void foo(char* left, char* rite, int element)
{
  while (left <= rite)
  {
    /* This should expand into
       D.zzzz = D.zzzz - D.xxxx;
       and NOT to
       D.D.yyyy = -D.xxxx; D.zzzz = D.zzzz + D.yyyy;  */
    rite -= element;
    bar(left, rite, element);
  }
}

/* There should be no " + " in the dump.  */
/* { dg-final { scan-tree-dump-times " \\\+ " 0 "reassoc2" } } */
/* { dg-final { cleanup-tree-dump "reassoc2" } } */
