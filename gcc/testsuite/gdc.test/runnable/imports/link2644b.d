module imports.link2644b;

import imports.link2644c;
import imports.link2644a;

version(X)
    struct X { alias C!(bool) CB; }
else
    alias C!(bool) CB;
