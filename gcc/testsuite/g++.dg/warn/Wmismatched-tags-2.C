// PR c++/93869 - ICE with -Wmismatched-tags.
// { dg-do compile }
// { dg-options "-Wmismatched-tags" }

namespace N { typedef int T; }
typename N::T x;
