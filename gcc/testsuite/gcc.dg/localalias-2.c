/* { dg-require-alias "" }  */
extern void abort (void);
int test2count;
__attribute__ ((weak,noinline))
void test(void)
{
  test2count++;
}
__attribute ((alias("test")))
static void test2(void);

void tt() 
{
  int prev = test2count;
  /* This call must bind locally.  */
  test2();
  if (test2count == prev)
    abort();
  test();
 }
