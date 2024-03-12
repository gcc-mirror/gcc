module pkg20008.subpkg.subsubmod;

import pkg20008.subpkg;
import pkg20008.subpkg.subsubmod;

enum T8 { x }

static assert([__traits(allMembers, pkg20008.subpkg)] == ["object", "pkg20008", "T9"]);
static assert([__traits(allMembers, pkg20008.subpkg.subsubmod)] == ["object", "pkg20008", "T8"]);
