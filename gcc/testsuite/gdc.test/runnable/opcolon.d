// REQUIRED_ARGS: -betterC

/* This test triggers an address miscalculation bug in DMD 2.111
 * with -inline.
 * May not crash if the data segment has a different layout,
 * e.g. when pasted into another file.
 */

struct S {
   int i;
}

__gshared S gs = S(1);
ref S get()
{
    return gs;
}

extern (C)
int main()
{
    (get().i ? get() : get()).i++;
    return 0;
}
