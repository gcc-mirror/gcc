module imports.link2644a;

import imports.link2644c;
import imports.link2644b;

version(X)
    struct X { alias C!(bool) CA; }
else
    alias C!(bool) CA;
