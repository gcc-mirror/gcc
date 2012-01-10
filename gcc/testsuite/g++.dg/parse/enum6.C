// PR c++/51587

namespace N
{
  struct X;			// { dg-message "previous declaration" }
}

enum N::X {};			// { dg-error "conflicting declaration" }
