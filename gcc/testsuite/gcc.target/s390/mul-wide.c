/* { dg-do compile } */
/* { dg-options "-O2 -m64 -fdump-tree-optimized" } */

__int128 foo(long long a, long long b)
{
   return (__int128)a * (__int128)b;
}

/* { dg-final { scan-tree-dump " w\\* " "optimized" } } */
