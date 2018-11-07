// REQUIRED_ARGS: -g
// PERMUTE_ARGS: -inline -version=A
// EXTRA_SOURCES: imports/link15194b.d imports/link15194std.d
// COMPILE_SEPARATELY: -g

import imports.link15194std;
import imports.link15194b;

void main()
{
    int[] foo;

    // fun() returns new RedBlackTree!int. But it's instantiated in
    // non-template non-root function, so it's left as speculative.
    setUnion(fun()[], foo);
}
