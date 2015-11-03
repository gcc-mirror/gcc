/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

unsigned int a, *b;
unsigned short c;
int d;

void 
fn1 ()
{
  b = &d;
  *b = c = a; 
  *b = d;
}

/* We should remove all loads but that from a.  */
/* { dg-final { scan-tree-dump-not "= \[dbc\];" "fre1" } } */
