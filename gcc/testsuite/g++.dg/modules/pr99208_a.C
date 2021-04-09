// PR 99208 typedef anonymous class
// { dg-additional-options {-Wno-pedantic -fmodules-ts} }
module;
# 5 "pr99208_a.C" 1
typedef struct {} __mbstate_t;
# 7 "" 2
export module hello:format;
// { dg-module-cmi {hello:format} }
export __mbstate_t v;
