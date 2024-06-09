// PR c++/109359
// { dg-additional-options -frounding-math }

// For a while we were emitting two doubles (4 .long directives) as the value
// of a float array; it should only be two .longs.

// { dg-final { scan-assembler-times "long" 2 { target x86_64-*-* } } }
float xs[] = {0.001914, 0.630539};
