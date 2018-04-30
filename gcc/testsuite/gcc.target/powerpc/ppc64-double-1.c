// { dg-do compile }
/* { dg-skip-if "" { powerpc*-*-aix* } } */
// { dg-options "-O2 -mpowerpc64" }
// { dg-final { scan-assembler-not "stfd"  }  }

// The register allocator should have allocated the temporary long long value in a floating point register.

double
d2ll2d (double d)
{
        return (double)(long long)d;
}
