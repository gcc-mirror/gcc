// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }




// Make line numbers distinct from macro-2_a.H






#ifndef MACRO_2b_H
#define MACRO_2b_H

#define FOO_OK foo
#define BAR_OK(BAZ) BINKY(2)

#define FOO_BAD foot
#define BAR_BAD(BAZ) BINKY(3)


#endif
