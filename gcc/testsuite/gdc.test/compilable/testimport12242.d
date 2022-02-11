// PERMUTE_ARGS:
// EXTRA_FILES: imports/imp12242a.d imports/imp12242a1.d imports/imp12242a2.d imports/imp12242b.d imports/imp12242b1.d imports/imp12242b2.d
module testimport12242;

import imports.imp12242a;   // test         // stripA == OverloadSet
import imports.imp12242a1;  // std.string   // stripA == template

import imports.imp12242b1;  // std.string   // stripB == template
import imports.imp12242b;   // test         // stripB == OverloadSet

void main()
{
    static assert(stripA(" af ") == 1);
    static assert(" af ".stripA() == 1);    // UFCS (1)
    static assert(" af ".stripA == 1);      // UFCS (2)

    static assert(stripB(" af ") == 1);
    static assert(" af ".stripB() == 1);    // UFCS (1)
    static assert(" af ".stripB == 1);      // UFCS (2)


    static assert(foo!int   == 1);
    static assert(foo!long  == 2);
    static assert(foo!float == 3);
    static assert(foo!real  == 4);
}
