// Bug: g++ is wrongfully pedantic about union initializers.
// Build don't link:

union U { int mbr; } array[1] = { 0 };
