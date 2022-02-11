// EXTRA_FILES: imports/plainpackage/plainmodule.d imports/pkgmodule/package.d imports/pkgmodule/plainmodule.d
module test.compilable.test16002;

import imports.plainpackage.plainmodule;
import imports.pkgmodule.plainmodule;

struct MyStruct;

alias a = imports.plainpackage;
alias b = imports.pkgmodule.plainmodule;

static assert(is(imports.plainpackage == package));
static assert(is(a == package));
static assert(!is(imports.plainpackage.plainmodule == package));
static assert(!is(b == package));
static assert(is(imports.pkgmodule == package));
static assert(!is(MyStruct == package));

static assert(!is(imports.plainpackage == module));
static assert(!is(a == module));
static assert(is(imports.plainpackage.plainmodule == module));
static assert(is(b == module));
// This is supposed to work even though we haven't directly imported imports.pkgmodule.
static assert(is(imports.pkgmodule == module));
static assert(!is(MyStruct == module));

static assert(!is(imports.nonexistent == package));
static assert(!is(imports.nonexistent == module));
