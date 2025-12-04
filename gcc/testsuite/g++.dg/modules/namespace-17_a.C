// PR c++/122995
// { dg-additional-options "-fmodules" }
// { dg-module-cmi bug2 }

export module bug2;
namespace N {
  namespace C { struct r3_cpo; } export extern inline C::r3_cpo const r3;
}
