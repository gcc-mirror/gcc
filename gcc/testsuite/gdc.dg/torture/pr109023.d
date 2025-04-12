// { dg-do "compile" }
// { dg-additional-options "-I[srcdir] -finclude-imports" }
// { dg-additional-files "imports/pr109023.d" }
// { dg-final { scan-assembler "_D7imports8pr1090237f109023FZv" } }
module pr109023;
import imports.pr109023;
