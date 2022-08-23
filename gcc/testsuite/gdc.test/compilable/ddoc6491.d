// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh

module ddoc6491;

import core.cpuid;

enum int c6491 = 4;

/// test
void bug6491a(int a = ddoc6491.c6491, string b = core.cpuid.vendor);
