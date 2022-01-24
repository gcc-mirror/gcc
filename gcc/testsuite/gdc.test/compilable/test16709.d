// EXTRA_FILES: imports/test16709a.d imports/test16709b.d imports/test16709c.d imports/test16709d.d
// https://issues.dlang.org/show_bug.cgi?id=16709

import imports.test16709a;
import imports.test16709b;

void test(){
  1.to!int;
}
