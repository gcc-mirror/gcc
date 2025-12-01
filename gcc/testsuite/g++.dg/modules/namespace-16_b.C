// PR c++/122915
// { dg-additional-options "-fmodules" }
// { dg-module-cmi tests:part }

export module tests:part;
namespace abc {}
namespace part {
  export using namespace abc;
}
