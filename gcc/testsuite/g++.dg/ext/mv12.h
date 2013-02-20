// Header file used by mv12.C and mv12-aux.cc.

int foo () __attribute__ ((target ("default")));
int foo () __attribute__ ((target ("sse4.2")));
