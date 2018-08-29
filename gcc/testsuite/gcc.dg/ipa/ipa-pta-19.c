/* { dg-do run } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O2 -fipa-pta" } */

#include <stdlib.h>
#include <stdbool.h>

#define hot __attribute__((hot))

static hot void multiver_default(unsigned int in, bool *ret)
{
	if ( in & 1 ) {
		*ret = false;
	}else{
		*ret = true;
	}
}

static void (*resolve_multiver(void))(unsigned int in, bool *out)
{
	return &multiver_default;
}

__attribute__ ((ifunc("resolve_multiver")))
static void multiver_test(unsigned int val, bool *ret);

static hot bool do_test(unsigned int val)
{
	bool ret = false;

	multiver_test(val, &ret);

	return (ret == !(val & 0x1));
}

volatile unsigned int x = 2;
int main()
{
  int i;
  for(i = 1; i < x; i++) {
      unsigned int val = x;
      if ( !do_test(val) )
	abort ();
  }
  return 0;
}
