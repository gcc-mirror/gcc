import imports.test11563std_traits;

interface J : I {} // comment out to let compilation succeed

struct A { }
pragma(msg, moduleName!A);


interface I {}
