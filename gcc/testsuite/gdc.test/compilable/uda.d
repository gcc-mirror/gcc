/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=15180
// [REG2.069.0-b1] Segfault with empty struct used as UDA

struct foo { }
@foo bar () { }

/************************************************/

// https://issues.dlang.org/show_bug.cgi?id=23241

alias feynman = int;
enum get = __traits(getAttributes, feynman);
static assert(get.length == 0);
