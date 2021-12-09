// REQUIRED_ARGS: -de
// COMPILED_IMPORTS: imports/g313.d
// EXTRA_FILES: imports/g313public.d imports/g313staticif.d imports/g313stringmixin.d imports/g313templatemixin.d
import imports.g313;

void test15900()
{
    // publically imported modules from g313 should be available here
    imports.g313public.bug();
    imports.g313staticif.bug();
    imports.g313stringmixin.bug();
    imports.g313templatemixin.bug();
}
