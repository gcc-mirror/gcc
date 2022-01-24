// PERMUTE_ARGS:
// REQUIRED_ARGS: -Jrunnable/extra-files
// EXTRA_FILES: extra-files/foo37.txt extra-files/std14198/uni.d

import core.stdc.stdio;

void main()
{
    auto s = import("foo37.txt");
    printf("%.*s\n", cast(int)s.length, s.ptr);
    // also want to ensure that we can access
    // imports in a subdirectory of the -J path
    s = import("std14198/uni.d");
    printf("%.*s\n", cast(int)s.length, s.ptr);
}
