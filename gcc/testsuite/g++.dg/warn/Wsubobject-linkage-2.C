// PR c++/53184
// { dg-options "-Wno-subobject-linkage" }

typedef volatile struct { } Foo;

#line 7 "foo.C"
struct Bar { Foo foo; };
struct Bar2 : Foo { };
