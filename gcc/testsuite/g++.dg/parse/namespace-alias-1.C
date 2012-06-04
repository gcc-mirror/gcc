// PR c++/26155

namespace N
{
  namespace M = N;  // { dg-error "previous declaration" }
  namespace M {}    // { dg-error "declaration of namespace" }
}
