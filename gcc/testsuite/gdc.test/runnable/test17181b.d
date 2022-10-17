// EXTRA_SOURCES: imports/test17181a.d
// EXTRA_FILES: imports/test17181c.d
module test17181b;

import imports.test17181c; // only imported, not compiled
                           // => must not be in ModuleInfo.importedModules

static this()
{
    // By instantiating the getA template, its local imports.test17181a
    // import is added to this module (not to imports.test17181c), and its
    // module ctor must have run already.
    assert(imports.test17181c.getA!() == 1);
}

void main() {}
