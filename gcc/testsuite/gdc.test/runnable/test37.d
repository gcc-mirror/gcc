// PERMUTE_ARGS:
// REQUIRED_ARGS: -Jrunnable/extra-files
// EXTRA_FILES: extra-files/foo37.txt extra-files/std14198/uni.d

import std.stdio;

void main()
{
    writefln(import("foo37.txt"));
    // also want to ensure that we can access
    // imports in a subdirectory of the -J path
    writefln(import("std14198/uni.d"));
}
