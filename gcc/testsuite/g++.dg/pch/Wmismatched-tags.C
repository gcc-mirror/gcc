/*  PR c++/61339 - add mismatch between struct and class
    Verify that declarations that don't match definitions in precompiled
    headers are diagnosed.
    { dg-options "-Wall -Wmismatched-tags" } */

#include "Wmismatched-tags.H"

class PCHDeclaredClass;
struct PCHDeclaredStruct;

struct PCHDefinedClass;       // { dg-warning "declared with a mismatched class-key 'struct'" }
class PCHDefinedStruct;       // { dg-warning "declared with a mismatched class-key 'class'" }

class PCHDeclaredClass { };
struct PCHDeclaredStruct { };
