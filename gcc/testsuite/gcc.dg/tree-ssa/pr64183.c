/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-vectorize -fdisable-tree-sccp -fdump-tree-cunroll-details" } */

int bits;
unsigned int size;
int max_code;

void
test ()
{
 int code = 0;

 while (code < max_code)
   code |= ((unsigned int) (size >> (--bits)));

 while (bits < (unsigned int)25)
   bits += 8;
}

/* { dg-final { scan-tree-dump "Loop 2 iterates at most 3 times" "cunroll"} } */
