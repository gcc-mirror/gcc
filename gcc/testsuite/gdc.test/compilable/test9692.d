// EXTRA_FILES: test9692a.d imports/test9692b.d
module test9692;

import test9692a;
import imports.test9692b;

enum x = [__traits(allMembers, imports.test9692b)];  // ok
enum y = [__traits(allMembers, test9692a)];  // ng: should work
