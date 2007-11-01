// { dg-do assemble }
// { dg-xfail-if "" { i386-pc-solaris* } { "*" } { "" } }

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>

// Fails with dwarf debugging.

typedef __java_boolean jboolean;
void foo() {}
