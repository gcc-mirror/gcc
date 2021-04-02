// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }
namespace std { class type_info; }

import "pr99245_a.H";

namespace std {
  const type_info* __cxa_exception_type () noexcept;
}
