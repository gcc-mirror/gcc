// REQUIRED_ARGS: -de
module imports.test314; // package imports

import imports.a314;

void main()
{
    imports.a314.bug("This should work.\n");
    renamed.bug("This should work.\n");
    bug("This should work.\n");
}
