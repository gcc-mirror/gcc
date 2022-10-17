/* { dg-do compile } */
/* { dg-options "-O2 -m64 -fdump-tree-optimized" } */

unsigned __int128 foo(unsigned long long a, unsigned long long b)
{
   return (unsigned __int128)a * (unsigned __int128)b;
}

/* { dg-final { scan-tree-dump " w\\* " "optimized" } } */
