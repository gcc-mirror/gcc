/* { dg-do compile { target mips*-*-* sparc*-*-* } } */
/* { dg-options "-O2 -fdump-rtl-combine" } */

long long
load1 (int data)
{
    return (long long) data << 12;
}

/* { dg-final { scan-rtl-dump-not "ior" "combine" } } */
