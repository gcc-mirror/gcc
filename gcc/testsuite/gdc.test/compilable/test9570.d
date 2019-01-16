// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

void main()
{
    ubyte[256] data;

    foreach (immutable        i; 0..256) data[i] = i;
    foreach (    const        i; 0..256) data[i] = i;
    foreach (                 i; 0..256) static assert(!__traits(compiles, (data[i] = i)));
    foreach (immutable int    i; 0..256) data[i] = i;
    foreach (    const int    i; 0..256) data[i] = i;
    foreach (          int    i; 0..256) static assert(!__traits(compiles, (data[i] = i)));
    foreach (immutable(int)   i; 0..256) data[i] = i;
    foreach (    const(int)   i; 0..256) data[i] = i;
    foreach (          int    i; 0..256) static assert(!__traits(compiles, (data[i] = i)));
    foreach (immutable(ulong) i; 0..256) data[i] = i;
    foreach (    const(ulong) i; 0..256) data[i] = i;
    foreach (          ulong  i; 0..256) static assert(!__traits(compiles, (data[i] = i)));

    foreach (immutable        i, x; data) data[i] = i;
    foreach (    const        i, x; data) data[i] = i;
    foreach (                 i, x; data) static assert(!__traits(compiles, (data[i] = i)));
    foreach (immutable int    i, x; data) data[i] = i;
    foreach (    const int    i, x; data) data[i] = i;
    foreach (          int    i, x; data) static assert(!__traits(compiles, (data[i] = i)));
    foreach (immutable(int)   i, x; data) data[i] = i;
    foreach (    const(int)   i, x; data) data[i] = i;
    foreach (          int    i, x; data) static assert(!__traits(compiles, (data[i] = i)));
    foreach (immutable(ulong) i, x; data) data[i] = i;
    foreach (    const(ulong) i, x; data) data[i] = i;
    foreach (          ulong  i, x; data) static assert(!__traits(compiles, (data[i] = i)));

    foreach_reverse (immutable        i; 0..256) data[i] = i;
    foreach_reverse (    const        i; 0..256) data[i] = i;
    foreach_reverse (                 i; 0..256) static assert(!__traits(compiles, (data[i] = i)));
    foreach_reverse (immutable int    i; 0..256) data[i] = i;
    foreach_reverse (    const int    i; 0..256) data[i] = i;
    foreach_reverse (          int    i; 0..256) static assert(!__traits(compiles, (data[i] = i)));
    foreach_reverse (immutable(int)   i; 0..256) data[i] = i;
    foreach_reverse (    const(int)   i; 0..256) data[i] = i;
    foreach_reverse (          int    i; 0..256) static assert(!__traits(compiles, (data[i] = i)));
    foreach_reverse (immutable(ulong) i; 0..256) data[i] = i;
    foreach_reverse (    const(ulong) i; 0..256) data[i] = i;
    foreach_reverse (          ulong  i; 0..256) static assert(!__traits(compiles, (data[i] = i)));

    foreach_reverse (immutable        i, x; data) data[i] = i;
    foreach_reverse (    const        i, x; data) data[i] = i;
    foreach_reverse (                 i, x; data) static assert(!__traits(compiles, (data[i] = i)));
  //foreach_reverse (immutable int    i, x; data) data[i] = i;
  //foreach_reverse (    const int    i, x; data) data[i] = i;
  //foreach_reverse (          int    i, x; data) static assert(!__traits(compiles, (data[i] = i)));
  //foreach_reverse (immutable(int)   i, x; data) data[i] = i;
  //foreach_reverse (    const(int)   i, x; data) data[i] = i;
  //foreach_reverse (          int    i, x; data) static assert(!__traits(compiles, (data[i] = i)));
    foreach_reverse (immutable(ulong) i, x; data) data[i] = i;
    foreach_reverse (    const(ulong) i, x; data) data[i] = i;
    foreach_reverse (          ulong  i, x; data) static assert(!__traits(compiles, (data[i] = i)));
}
