import imports.udamodule2;
import imports.udamodule2a;

enum Attrib = __traits(getAttributes, imports.udamodule2);
static assert(Attrib[0] == UDA(1) && Attrib[1] == UDA(2));
