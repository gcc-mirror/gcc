// https://issues.dlang.org/show_bug.cgi?id=22632

static assert(["one": 1] != null);
static assert(null != ["one": 1]);
