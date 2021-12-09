// REQUIRED_ARGS: -de
// EXTRA_FILES: imports/a314.d imports/c314.d
module imports.test314; // package imports

import imports.a314;

void main()
{
    imports.a314.bug("This should work.\n");
    renamed.bug("This should work.\n");
    bug("This should work.\n");
}
