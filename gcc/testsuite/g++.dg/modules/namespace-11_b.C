// { dg-additional-options "-fmodules" }

export module M2;
export import M1;

namespace A
{
  export using A::AT;
}
