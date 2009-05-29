/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR36598 AVR fail maybe due to cost metrics */
/* { dg-final { scan-tree-dump-times "nasty_local" 0 "optimized" { xfail { "avr-*-*" } } } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
struct a {int a,b,c;} a;
int test(struct a a)
{
struct a nasty_local;
__builtin_memcpy (&nasty_local,&a, sizeof(a));
return nasty_local.a;
}
