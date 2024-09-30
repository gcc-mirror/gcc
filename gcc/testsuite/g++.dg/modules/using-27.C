// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi !bad }

module;

static int x = 123;   // { dg-message "declared here with internal linkage" }
static void f() {}    // { dg-message "declared here with internal linkage" }
using T = struct {};  // { dg-message "declared here with no linkage" }

export module bad;

export using ::x;  // { dg-error "does not have external linkage" }
export using ::f;  // { dg-error "does not have external linkage" }
export using ::T;  // { dg-error "does not have external linkage" }
