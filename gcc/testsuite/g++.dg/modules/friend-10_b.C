// PR c++/122646
// { dg-additional-options "-fmodules -fconcepts" }

import M;
template void foo<int>();
