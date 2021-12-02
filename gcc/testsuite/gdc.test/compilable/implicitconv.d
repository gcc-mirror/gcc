enum __c_wchar_t : wchar;

alias wchar_t = __c_wchar_t;

immutable(wchar_t)[] a = "somestring";
const(wchar_t)[]     b = "somestring";
immutable(wchar_t)*  c = "somestring";
const(wchar_t)*      d = "somestring";

string foo = "foo";

static assert(!__traits(compiles, { immutable(wchar_t)[] bar = foo; } ));
static assert(!__traits(compiles, { const(wchar_t)[]     bar = foo; } ));
static assert(!__traits(compiles, { immutable(wchar_t)*  bar = foo; } ));
static assert(!__traits(compiles, { const(wchar_t)*      bar = foo; } ));

// https://issues.dlang.org/show_bug.cgi?id=17141
static assert(is(typeof(true ? char.init : char.init) == char));
static assert(is(typeof(true ? char.init : wchar.init) == dchar));
static assert(is(typeof(true ? char.init : dchar.init) == dchar));
static assert(is(typeof(true ? wchar.init : wchar.init) == wchar));
static assert(is(typeof(true ? wchar.init : dchar.init) == dchar));
static assert(is(typeof(true ? dchar.init : dchar.init) == dchar));

enum cenum : char { a }
enum wenum : wchar{ b }
enum denum : dchar{ c }

static assert(is(typeof(true ? char.init : cenum.init) == char));
static assert(is(typeof(true ? wchar.init : cenum.init) == dchar));
static assert(is(typeof(true ? char.init : wenum.init) == dchar));
static assert(is(typeof(true ? dchar.init : wenum.init) == dchar));
static assert(is(typeof(true ? cenum.init : wenum.init) == dchar));
