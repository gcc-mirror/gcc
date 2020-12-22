// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

#ifndef MACRO_2a_H
#define MACRO_2a_H

#define FOO_OK foo
#define BAR_OK(BAZ) BINKY(2)

#define FOO_BAD foo
#define BAR_BAD(BAZ) BINKY(2)


#endif
