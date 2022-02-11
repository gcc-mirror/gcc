/*
EXTRA_FILES: imports/ice7782algorithm.d imports/ice7782range.d
TEST_OUTPUT:
----
fail_compilation/ice7782.d(14): Error: unable to read module `ice7782math`
fail_compilation/ice7782.d(14):        Expected 'imports/ice7782range/imports/ice7782math.d' or 'imports/ice7782range/imports/ice7782math/package.d' in one of the following import paths:
import path[0] = fail_compilation
import path[1] = $p:druntime/import$
import path[2] = $p:phobos$
----
*/

import imports.ice7782algorithm;
import imports.ice7782range. imports.ice7782math;

void main() {}
