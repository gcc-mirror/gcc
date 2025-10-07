/* { dg-do compile }  */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0 -fdump-ipa-targetclone1-details" } */

int foo [[gnu::target_version("dotprod;priority=1")]] (void);
int foo [[gnu::target_version("default")]] (void) { return 1; }
int foo [[gnu::target_version("dotprod;priority=1")]] (void) { return 2; }
int foo [[gnu::target_version("sve")]] (void) { return 3; }

// Priority1 dotprod > sve
/* { dg-final { scan-ipa-dump-times "Version order for foo/\[0-9\]+:\\nfoo\.default/\[0-9\]+\\nfoo\._Msve/\[0-9\]+\\nfoo\._Mdotprod/\[0-9\]+\\n" 1 "targetclone1" } } */

int bar [[gnu::target_version("dotprod;priority=2")]] (void);
int bar [[gnu::target_version("default")]] (void) { return 1; }
int bar [[gnu::target_version("dotprod")]] (void) { return 2; }
int bar [[gnu::target_version("sve;priority=1")]] (void) { return 3; }

// Priority2 dotprod > Priority1 sve
/* { dg-final { scan-ipa-dump-times "Version order for bar/\[0-9\]+:\\nbar\.default/\[0-9\]+\\nbar\._Msve/\[0-9\]+\\nbar\._Mdotprod/\[0-9\]+\\n" 1 "targetclone1" } } */

int baz [[gnu::target_version("default")]] (void) { return 1; }
int baz [[gnu::target_version("dotprod;priority=1")]] (void) { return 2; }
int baz [[gnu::target_version("sve;priority=1")]] (void) { return 3; }

// Priority1 sve > Priority1 dotprod
/* { dg-final { scan-ipa-dump-times "Version order for baz/\[0-9\]+:\\nbaz\.default/\[0-9\]+\\nbaz\._Mdotprod/\[0-9\]+\\nbaz\._Msve/\[0-9\]+\\n" 1 "targetclone1" } } */
