// PR c++/26155

namespace N
{
  namespace M = N;  // { dg-message "previous declaration" }
  namespace M {}    // { dg-error "declaration of namespace" }
}
