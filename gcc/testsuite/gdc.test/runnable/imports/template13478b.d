import imports.template13478b;

import imports.template13478a;

// Note that foo is only used in the template constraint here.
T barImpl(T)(T t) if (is(typeof({ foo!T(); }))) { return t; }
int bar(int a) { return barImpl(a); }
