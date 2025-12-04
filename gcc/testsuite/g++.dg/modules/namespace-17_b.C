// PR c++/122995
// { dg-additional-options "-fmodules" }
// { dg-module-cmi bug }

export module bug;
import bug2;
namespace N {
  namespace C { struct r1_cpo; } export extern inline C::r1_cpo const r1;
  namespace C { struct r2_cpo; } export extern inline C::r2_cpo const r2;
}
