/* { dg-do run } */
/* { dg-options "-O2" } */
extern void abort (void);
unsigned global_iters;

void bi_reverse(int len)
{
    do {
	global_iters++;
    } while (--len > 0);
}

int main()
{
  bi_reverse(5);
  if (global_iters != 5)
    abort ();
  return 0;
}
