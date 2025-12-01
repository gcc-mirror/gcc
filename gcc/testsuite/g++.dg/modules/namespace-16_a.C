// PR c++/122915
// { dg-additional-options "-fmodules" }
// { dg-module-cmi imagine }

export module imagine;
namespace ns {}
export namespace ig {
  using namespace ns;
}
