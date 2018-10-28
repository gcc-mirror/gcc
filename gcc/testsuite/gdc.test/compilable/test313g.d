// REQUIRED_ARGS: -de
// EXTRA_SOURCES: imports/g313.d
import imports.g313;

void test15900()
{
    // publically imported modules from g313 should be available here
    imports.g313public.bug();
    imports.g313staticif.bug();
    imports.g313stringmixin.bug();
    imports.g313templatemixin.bug();
}
