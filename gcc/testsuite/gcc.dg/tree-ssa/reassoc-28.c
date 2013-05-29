/* { dg-do run} */
/* { dg-options "-O2 -fdump-tree-reassoc1-details" } */

#define LENGTH 4
void abort (void);
unsigned
__attribute__ ((noinline))  foo (unsigned char *buf, int n)
{
 unsigned sum = 0, i = 0;
 do {
   sum +=(buf)[n-1];
   /* Split the BB to test statements are correctly moved to
      satisfy dependences.  */
   if (n > LENGTH)
     i++;
   sum += buf[n-2];
   sum += buf[n-3];
   sum += buf[n-4];
   n = n-4;
 } while (n > 0);

 return sum + i;
}

unsigned char a[] = {1, 1, 1, 1};

int main() {
  int sum = foo (a, LENGTH);
  if (sum != 4)
    abort ();
  return 0;
}

/* Verify one stmt has been moved to another BB to ensure correct dependences.  */
/* { dg-final { scan-tree-dump-times "to a different BB" 1 "reassoc1"} }*/
/* { dg-final { scan-tree-dump-times "within same BB" 2 "reassoc1"} }*/
/* { dg-final { cleanup-tree-dump "reassoc1" } } */
