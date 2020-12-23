// { dg-do preprocess }

module bob;
#if 1
export import stuart;
#else
import kevin;
#endif
import gru;
#define EXPORT
EXPORT import mabel;
int i;

// { dg-final { scan-file cpp-1.i "cpp-1.C\"\n\n\nmodule bob;\n\nexport import stuart;\n\n\n\nimport gru;\n\n import mabel;\n" } }
