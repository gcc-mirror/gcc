// { dg-do preprocess }
// { dg-additional-options -fmodule-preamble }

module bob;
#if 1
export import stuart;
#else
import kevin;
#endif
import gru;
#define EXPORT // { dg-warning "preamble ended immediately before" }
EXPORT import mabel;
int i;

// { dg-final { scan-file cpp-preamble-1.i "cpp-preamble-1.C\"\n\n\n\nmodule bob;\n\nexport import stuart;\n\n\n\nimport gru;" } }
// { dg-final { scan-file-not cpp-preamble-1.i {import kevin;} } }
// { dg-final { scan-file-not cpp-preamble-1.i {import mabel;} } }
