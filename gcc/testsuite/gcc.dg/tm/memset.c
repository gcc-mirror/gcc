/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

extern void *memset (void *, int, __SIZE_TYPE__);

char array[4] = "aaaa";

__attribute__((transaction_safe))
void *my_memset()
{
  return memset(array,'b',4);
}


int main()
{

	__transaction_atomic {
		my_memset();
	}
	return 0;
}

/* { dg-final { scan-assembler "_ITM_memsetW" } } */
