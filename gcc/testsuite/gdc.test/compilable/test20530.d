// EXTRA_FILES: imports/test20530a.d imports/plainpackage/plainmodule.d imports/pkgmodule/package.d imports/pkgmodule/plainmodule.d
module mod;
static assert(is(mod == module));
static assert(is(mixin("mod") == module));
static assert(!is(mod == package));
static assert(!is(mixin("mod") == package));

import imports.test20530a;
static assert(is(imports == package));
static assert(is(mixin("imports") == package));
static assert(!is(imports == module));
static assert(!is(mixin("imports") == module));

import imports.plainpackage.plainmodule;
import imports.pkgmodule.plainmodule;

struct MyStruct;

alias a = mixin("imports.plainpackage");
alias b = mixin("imports.pkgmodule.plainmodule");

static assert(is(mixin("imports.plainpackage") == package));
static assert(is(mixin("a") == package));
static assert(!is(mixin("imports.plainpackage.plainmodule") == package));
static assert(!is(mixin("b") == package));
static assert(is(mixin("imports.pkgmodule") == package));
mixin("static assert(is(imports.pkgmodule == package));");

static assert(!is(mixin("MyStruct") == package));

static assert(!is(mixin("imports.plainpackage") == module));
static assert(!is(mixin("a") == module));
static assert(is(mixin("imports.plainpackage.plainmodule") == module));
static assert(is(mixin("b") == module));
static assert(is(mixin("imports.pkgmodule") == module));
mixin("static assert(is(imports.pkgmodule == module));");

static assert(!is(mixin("MyStruct") == module));

static assert(!is(mixin("imports.nonexistent") == package));
static assert(!is(mixin("imports.nonexistent") == module));

// this won't work due to mixin argument .stringof expansion,
// it will expand to mixin(package imports.pkgmodule). Issue 20519.
//static assert(is(mixin(imports.pkgmodule) == package));
//static assert(is(mixin(imports.pkgmodule) == module));
