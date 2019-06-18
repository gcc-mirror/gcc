/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=15180
// [REG2.069.0-b1] Segfault with empty struct used as UDA

struct foo { }
@foo bar () { }

/************************************************/
