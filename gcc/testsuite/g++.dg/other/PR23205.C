/* { dg-do compile } */
/* { dg-skip-if "No stabs" { mmix-*-* *-*-aix* alpha*-*-* hppa*64*-*-* ia64-*-* tile*-*-* *-*-vxworks } { "*" } { "" } } */
/* { dg-options "-gstabs+ -fno-eliminate-unused-debug-types" } */

const int foobar = 4;
int foo ()
{
        return foobar + 1;
}

int main()
{
        int i;
        i = foo();
        return i;
}

/* { dg-final { scan-assembler ".stabs.*foobar:(c=i|S)" } } */
