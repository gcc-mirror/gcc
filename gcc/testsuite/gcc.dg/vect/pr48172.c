/* { dg-do run } */

extern void *memset(void *s, int c, __SIZE_TYPE__ n);
extern void abort (void);

#define ASIZE 1028
#define HALF (ASIZE/2)

int main() {
  unsigned int array[ASIZE];
  int i;

  memset(array, 0, sizeof(array));

  /* initialize first half of the array */
  for (i = 0; i < HALF; i++)
    array[i] = i;

  /* fill second half of array in by summing earlier elements of the array
     gcc 4.5.1 and 4.5.2 incorrectly vectorize this loop!  aray[1025] is left
     at 0 for ASIZE=1028 */
  for (i = 0; i < HALF-1; i++)
    array[HALF+i] = array[2*i] + array[2*i + 1];

  /* see if we have any failures */
  for (i = 0; i < HALF - 1; i++)
    if (array[HALF+i] != array[2*i] + array[2*i + 1])
      abort ();

  return 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
