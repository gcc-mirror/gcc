// { dg-do assemble  }
// Bug: g++ is wrongfully pedantic about union initializers.

union U { int mbr; } array[1] = { 0 };
