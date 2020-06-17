// RUNNABLE_PHOBOS_TEST
// https://issues.dlang.org/show_bug.cgi?id=5305

import std.math;
void map(real function(real) f) { }
int main() { map(&sqrt); return 0; }


