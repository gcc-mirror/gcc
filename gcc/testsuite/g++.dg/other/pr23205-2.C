/* { dg-do compile } */
/* { dg-skip-if "No stabs" { aarch64*-*-* mmix-*-* *-*-aix* alpha*-*-* hppa*64*-*-* ia64-*-* tile*-*-* nios2-*-* } { "*" } { "" } } */
/* { dg-options "-gstabs+ -fno-eliminate-unused-debug-types -ftoplevel-reorder" } */

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

/* { dg-final { scan-assembler ".stabs.*foobar:c=i" } } */
