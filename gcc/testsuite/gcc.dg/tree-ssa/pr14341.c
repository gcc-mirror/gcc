/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp" } */

void fn_call (int);
int h(int, int);
void t()
{
  int i;
  int x;
    for( i = 0; i < 100000000; i++ ){ 
 	fn_call (i < 100000000);
    }
}

/* { dg-final { scan-tree-dump-times "fn_call \\(1\\)" 1 "vrp" } } */
/* { dg-final { cleanup-tree-dump "vrp" } } */
