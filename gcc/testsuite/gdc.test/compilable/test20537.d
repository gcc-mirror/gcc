// REQUIRED_ARGS: -Icompilable/imports
// EXTRA_FILES: imports/pkg20537/package.d
import pkg20537;

static assert(is(pkg20537 == module));
static assert(__traits(isModule, pkg20537));
static assert(is(mixin("pkg20537") == module));
static assert(is(pkg20537 == package));
static assert(__traits(isPackage, pkg20537));
static assert(is(mixin("pkg20537") == package));
