module imports.template13478a;

import gcc.attribute;

@attribute("noinline") bool foo(T)() {
    // Make sure this is not inlined so template13478.o actually
    // needs to reference it.
    return false;
}
