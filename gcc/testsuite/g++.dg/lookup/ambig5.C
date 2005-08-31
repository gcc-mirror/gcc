// PR c++/13377
// Origin: Boris Kolpackov <boris@kolpackov.net>
// { dg-do compile }

namespace N
{
  namespace M {}    // { dg-error "declared" }
}

namespace M {}      // { dg-error "declared" }

using namespace N;
using namespace M;  // { dg-error "namespace-name|ambiguous" }
