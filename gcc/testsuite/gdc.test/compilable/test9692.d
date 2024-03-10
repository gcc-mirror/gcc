// EXTRA_FILES: test9692a.d imports/test9692b.d
module test9692;

import test9692a;
import imports.test9692b;

static assert([__traits(allMembers, imports.test9692b)] == ["object", "k"]); // ok
static assert([__traits(allMembers, test9692a)] == ["object", "j"]); // ng: should work
