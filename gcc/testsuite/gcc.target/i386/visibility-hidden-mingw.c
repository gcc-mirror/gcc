/* { dg-do link { target *-*-mingw* *-*-cygwin* } } */
/* { dg-require-dll "" } */
/* { dg-options "-shared -fvisibility=hidden -Wl,--output-def,visibility-hidden-mingw.def" } */

void __attribute__((visibility("default"))) exported_func(void) {}
void hidden_func(void) {}
void __attribute__((visibility("hidden"))) explicit_hidden_func(void) {}
void __attribute__((visibility("internal"))) internal_func(void) {}

/* exported_func has default visibility, so it should be exported.  */
/* { dg-final { scan-file visibility-hidden-mingw.def "(?n)^\\s*exported_func(?:\\s+@\[0-9\]+)?$" } } */

/* hidden_func gets hidden from -fvisibility=hidden, so it should not be
   auto-exported.  */
/* { dg-final { scan-file-not visibility-hidden-mingw.def "(?n)^\\s*hidden_func(?:\\s+@\[0-9\]+)?$" } } */

/* explicit_hidden_func is explicitly hidden, so it should not be
   auto-exported.  */
/* { dg-final { scan-file-not visibility-hidden-mingw.def "(?n)^\\s*explicit_hidden_func(?:\\s+@\[0-9\]+)?$" } } */

/* internal_func has internal visibility, so it should not be auto-exported.  */
/* { dg-final { scan-file-not visibility-hidden-mingw.def "(?n)^\\s*internal_func(?:\\s+@\[0-9\]+)?$" } } */

/* { dg-final { remove-build-file "visibility-hidden-mingw.def" } } */
