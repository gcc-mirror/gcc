// { dg-additional-options "-fmodules-ts -fopenacc" }
// { dg-require-effective-target pthread }

export module foo;
// { dg-module-cmi foo { target pthread } }

export inline void frob (unsigned (&ary)[64])
{
  int sum, i;
#pragma acc parallel
#pragma acc loop gang worker vector reduction (+:sum)
  for (i = 0; i < 64; i++)
    sum += ary[i];
  ary[0] += sum;
}
