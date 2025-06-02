// { dg-additional-options "-fmodules" }

export module M2;
import M1;

namespace A
{
  export using A::AT;
}
