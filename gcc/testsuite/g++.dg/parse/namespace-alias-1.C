// PR c++/26155

namespace N
{
  namespace M = N;  // { dg-message "previous declaration" }
  namespace M {}    // { dg-error "conflicts with a previous declaration" }
}

namespace A
{
  namespace B 
  {
    namespace C
    {
    }
  }

  namespace D = B::C;
  namespace D  // { dg-error "not allowed" }
  {
  }
}
