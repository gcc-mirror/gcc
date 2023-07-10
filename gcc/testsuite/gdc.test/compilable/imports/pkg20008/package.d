module pkg20008;

import pkg20008.submod;

enum T2 { y }
enum T3 { z }

static assert([__traits(allMembers, pkg20008)] == ["object", "pkg20008", "T2", "T3"]);
static assert([__traits(allMembers, pkg20008.submod)] == ["object", "T1"]);
