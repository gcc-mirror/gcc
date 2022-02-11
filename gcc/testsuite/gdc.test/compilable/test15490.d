// REQUIRED_ARGS: -o- -inline
// PERMUTE_ARGS:
// EXTRA_FILES: imports/imp15490a.d imports/imp15490b.d
module test15490;

import imports.imp15490a;
import imports.imp15490b;

void main()
{
    regex();
    listenTCP();
}
