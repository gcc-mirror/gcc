module pkg20008.subpkg;

import pkg20008.subpkg.subsubmod;

enum T9 { x }

static assert([__traits(allMembers, pkg20008.subpkg)] == ["object", "pkg20008", "T9"]);
