// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts -fdirectives-only -isystem [srcdir] -fno-canonical-system-headers" }
// a comment
module; // line
frob
export
import foo; // line
import 7;

im\
port \
sing;
// comment
import "dir-only-2_a.H";
import <dir-only-2_a.H>;
X
#if !X
#error "no X!"
#endif
export module bob;

export import q;

// { dg-final { scan-file dir-only-2_b.i {// a comment\nmodule ;\nfrob} } }
// { dg-final { scan-file dir-only-2_b.i {frob\nexport\nimport  foo;\nimport 7;} } }
// { dg-final { scan-file dir-only-2_b.i {import  "[^\n]*/dir-only-2_a.H";\nimport  "[^\n]*/dir-only-2_a.H";\nX} } }
// { dg-final { scan-file dir-only-2_b.i {export  module  bob;\n\nexport  import  q;} } }
// { dg-final { scan-file dir-only-2_b.i {import  sing;\n\n\n// comment} } }
