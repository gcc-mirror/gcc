// PR 99245 ICE writing out user of type_info
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

namespace std { class type_info {}; }
