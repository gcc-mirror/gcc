import imports.test11563std_traits;

interface J : I {} // comment out to let compilation succeed

struct A { }
static assert(moduleName!A == "b");


interface I {}
