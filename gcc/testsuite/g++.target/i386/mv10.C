// { dg-do assemble }
// { dg-options "" }

__attribute__((target ("popcnt"), used))
void foo (void)
{
}

__attribute__((target ("popcnt","avx"), used))
void foo (void)
{
}
