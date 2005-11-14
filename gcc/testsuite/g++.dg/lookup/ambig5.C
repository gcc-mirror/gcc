// PR c++/13377
// Origin: Boris Kolpackov <boris@kolpackov.net>
// { dg-do compile }

namespace N
{
  namespace M {}    // { dg-error "M" }
}

namespace M {}      // { dg-error "M" }

using namespace N;
using namespace M;  // { dg-error "namespace-name|ambiguous" }
