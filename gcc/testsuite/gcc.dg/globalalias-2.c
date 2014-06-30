/* { dg-require-alias "" }  */
int test2count;
extern void abort (void);
static
void test(void)
{
  test2count++;
}
__attribute__ ((weak,noinline))
__attribute ((alias("test")))
void test2(void);

void tt() 
{
  int prev = test2count;
  /* This call must bind locally.  */
  test();
  if (test2count == prev)
    abort();
  test2();
 }
