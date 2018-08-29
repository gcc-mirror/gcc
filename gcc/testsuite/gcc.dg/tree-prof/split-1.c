/* Test that we don't get a link-time error when using -fsplit-stack
   due to implicit enabling of -freorder-blocks-and-partition.  */
/* { dg-require-effective-target split_stack } */
/* { dg-options "-O2 -fsplit-stack" } */

extern unsigned int sleep (unsigned int);

#define SIZE 10000

const char *sarr[SIZE];
const char *buf_hot;
const char *buf_cold;

__attribute__((noinline))
void 
foo (int path)
{
  int i;
  if (path)
    {
      for (i = 0; i < SIZE; i++)
	sarr[i] = buf_hot;
    }
  else
    {
      for (i = 0; i < SIZE; i++)
	sarr[i] = buf_cold;
      sleep (0);
    }
}

int
main (int argc, char *argv[])
{
  int i;
  buf_hot =  "hello";
  buf_cold = "world";
  for (i = 0; i < 1000000; i++)
    foo (argc);
  return 0;
}
