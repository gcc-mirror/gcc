/* { dg-do compile { target aarch64*-*-* mips64*-*-* sparc64*-*-* } } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -fdump-rtl-combine" } */

__int128_t
load2 (int data)
{
    return (__int128_t) data << 50;
}

/* { dg-final { scan-rtl-dump-not "ior" "combine" } } */
