// EXTRA_FILES: imports/test11563core_bitop.d imports/test11563std_array.d imports/test11563std_range.d imports/test11563std_traits.d
import imports.test11563std_traits;

interface J : I {} // comment out to let compilation succeed

struct A { }
static assert(moduleName!A == "b");


interface I {}
