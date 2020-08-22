/* { dg-additional-options "-fanalyzer-checker=taint" } */

typedef __SIZE_TYPE__ size_t;

int idx;
void *fp;

size_t
fread (void *, size_t, size_t, void *);

void
ql (void)
{
  int n1[1];

  fread (n1, sizeof (n1[0]), 1, fp); /* { dg-message "'n1' gets an unchecked value here" "" { xfail *-*-* } } */
  idx = n1[0]; /* { dg-message "'idx' has an unchecked value here \\\(from 'n1'\\\)" "" { xfail *-*-* } } */
}

int arr[10];
	
int
pl (void)
{
  ql ();
  return arr[idx]; /* { dg-warning "use of tainted value 'idx' in array lookup without bounds checking" "" { xfail *-*-* } } */
}
