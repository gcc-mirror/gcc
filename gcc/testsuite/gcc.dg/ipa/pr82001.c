/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fdump-ipa-icf-details"  } */

int
mullo (int a, int b)
{
  asm("mul %%edx   # %%1 was %1"
      : "+"
	"a"(a),
	"+d"(b));
  return a;
}

int
mulhi (int a, int b)
{
  asm("mul %%edx   # %%1 was %1" : "+d"(a), "+a"(b));
  return a;
}

/* { dg-final { scan-ipa-dump "Equal symbols: 0" "icf"  } } */
