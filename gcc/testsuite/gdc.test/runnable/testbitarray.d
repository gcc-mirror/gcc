// RUNNABLE_PHOBOS_TEST
// PERMUTE_ARGS:

import std.bitmanip;

void main() {
        BitArray a;
        a.length = 5;
        foreach (ref bool b; a) {
                assert (b == 0);
                b = 1;
        }
        foreach (bool b; a)
                assert (b == 1); // FAILS, they're all 0
}


