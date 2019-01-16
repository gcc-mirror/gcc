/*
TEST_OUTPUT:
---
fail_compilation/diag14876.d(17): Deprecation: class diag14876.Dep is deprecated
fail_compilation/diag14876.d(18): Deprecation: class diag14876.Dep is deprecated
fail_compilation/diag14876.d(19): Deprecation: class diag14876.Dep is deprecated
fail_compilation/diag14876.d(20): Deprecation: class diag14876.Dep is deprecated
fail_compilation/diag14876.d(21): Deprecation: class diag14876.Dep is deprecated
fail_compilation/diag14876.d(22): Deprecation: class diag14876.Dep is deprecated
fail_compilation/diag14876.d(23): Deprecation: class diag14876.Dep is deprecated
fail_compilation/diag14876.d(23): Error: can only slice tuple types, not diag14876.Dep
---
*/

deprecated class Dep { class Mem {} }

alias X1 = Foo!(Dep[]);
alias X2 = Foo!(Dep[1]);
alias X3 = Foo!(Dep[int]);
alias X4 = Foo!(int[Dep]);
alias X5 = Foo!(Dep*);
alias X6 = Foo!(Dep.Mem);
alias X7 = Foo!(Dep[3..4]);

template Foo(T) {}
