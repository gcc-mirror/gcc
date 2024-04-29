// { dg-additional-options "-fmodules-ts" }

import X;
import Y;

// This should happily refer to the same S and T
// as already instantiated in both X and Y
A<long> az;

// And same for f and g
B<long> bz;
