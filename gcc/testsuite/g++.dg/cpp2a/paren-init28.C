// PR c++/92812
// P1975R0
// { dg-do compile { target c++20 } }

// In both cases the reference declarations lifetime-extend the array
// temporary.
int (&&r)[3] = static_cast<int[3]>(42);
int (&&r2)[1] = static_cast<int[]>(42);

// Make sure we've lifetime-extended.
// { dg-final { scan-assembler "_ZGR1r_" } }
// { dg-final { scan-assembler "_ZGR2r2_" } }

// Narrowing is probably OK here.
int (&&r3)[1] = static_cast<int[1]>(1.3);
