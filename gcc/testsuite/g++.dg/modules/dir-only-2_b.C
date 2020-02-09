// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts -fdirectives-only -isystem [srcdir]" }
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

// { dg-final { scan-file dir-only-2_b.i {// a comment\n__module;\nfrob} } }
// { dg-final { scan-file dir-only-2_b.i {frob\nexport\n__import foo;\nimport 7;} } }
// { dg-final { scan-file dir-only-2_b.i {__import "[^\n]*/dir-only-2_a.H";\n__import "[^\n]*/dir-only-2_a.H";\nX} } }
// { dg-final { scan-file dir-only-2_b.i {__export __module bob;\n\n__export __import q;} } }
// { dg-final { scan-file dir-only-2_b.i {__import sing;\n\n\n// comment} } }
