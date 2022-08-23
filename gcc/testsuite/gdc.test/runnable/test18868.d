/*
COMPILE_SEPARATELY:
EXTRA_SOURCES: imports/test18868_a.d imports/test18868_fls.d
PERMUTE_ARGS:
*/

import imports.test18868_fls;
alias floop = FLS!(int);
void main() {}
