// { dg-do compile }
// { dg-options "-O2 -mpowerpc64" }
// xfail: PR middle-end/37272
// { dg-final { scan-assembler-not "stfd" { xfail lp64 }  }  }

// The register allocator should have allocated the temporary long long value in a floating point register.

double
d2ll2d (double d)
{
        return (double)(long long)d;
}
