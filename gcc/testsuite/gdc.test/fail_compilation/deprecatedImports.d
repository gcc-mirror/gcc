/*
REQUIRED_ARGS: -de
EXTRA_FILES: imports/deprecatedImporta.d imports/deprecatedImportb.d

TEST_OUTPUT:
----
fail_compilation/deprecatedImports.d(19): Deprecation: alias `deprecatedImporta.foo` is deprecated - Please import deprecatedImportb directly!
fail_compilation/deprecatedImports.d(21): Deprecation: alias `deprecatedImporta.bar` is deprecated - Please import deprecatedImportb directly!
fail_compilation/deprecatedImports.d(23): Deprecation: alias `deprecatedImporta.AliasSeq` is deprecated - Please import deprecatedImportb directly!
fail_compilation/deprecatedImports.d(27): Deprecation: alias `deprecatedImporta.S` is deprecated - Please import deprecatedImportb directly!
fail_compilation/deprecatedImports.d(29): Deprecation: alias `deprecatedImporta.C` is deprecated - Please import deprecatedImportb directly!
fail_compilation/deprecatedImports.d(31): Deprecation: alias `deprecatedImporta.I` is deprecated - Please import deprecatedImportb directly!
fail_compilation/deprecatedImports.d(25): Deprecation: alias `deprecatedImporta.E` is deprecated - Please import deprecatedImportb directly!
----
*/

import imports.deprecatedImporta;

alias f = foo;

alias b = bar!(int);

alias Types = AliasSeq!(int);

int x = E;

S s;

C c;

I i;
