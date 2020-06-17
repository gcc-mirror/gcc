/* { dg-do compile } */

volatile __attribute__((uncached))  int * status =
  (volatile __attribute__((uncached)) int *) 0x04 ;

int get_stat (void)
{
  return *status;
}

/* { dg-final { scan-assembler-times "ld\.di" 2 } } */
