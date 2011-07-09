/* { dg-options "-O2" } */
/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */

int test(long long a, long long b)
{
        return a * b;
}

/* Check that we did not spill anything.  This is all that is needed
   to qualify the generated code as "decent"...  */

/* { dg-final { scan-assembler-not "%e\[sd\]i" } } */
