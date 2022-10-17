// REQUIRED_ARGS: -o-
// EXTRA_FILES: protection/basic/mod1.d protection/basic/tests.d protection/subpkg/explicit.d protection/subpkg/tests.d protection/subpkg2/tests.d
// PERMUTE_ARGS:
import protection.basic.tests;
import protection.subpkg.tests;
import protection.subpkg2.tests;
