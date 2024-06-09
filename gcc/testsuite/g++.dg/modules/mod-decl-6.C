// PR c++/115200
// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi !M }

module;

namespace ns {  // { dg-message "scope opened here" }

export module M;  // { dg-error "global scope" }

}
