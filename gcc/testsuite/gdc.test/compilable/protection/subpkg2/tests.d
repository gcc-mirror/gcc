module protection.subpkg2.tests;

import pkg = protection.subpkg.explicit;

static assert (is(typeof(pkg.commonAncestorFoo())));
