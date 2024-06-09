// { dg-additional-options "-fmodules-ts" }

module;

module :foo;  // { dg-error "expected module-name" }

import :foo;  // { dg-error "import specifying a module-partition must appear after a named module-declaration" }
