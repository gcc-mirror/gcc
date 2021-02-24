# 0 "REALNAME"
# 0 "<built-in>"
# 0 "<command-line>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "REALNAME"

// { dg-additional-options {-fmodule-header -fpreprocessed -fdump-lang-module-lineno} }
// { dg-final { scan-lang-dump { 4 source file names\n Source file\[0\]=REALNAME\n Source file\[1\]=<built-in>\n Source file\[2\]=<command-line>\n Source file\[3\]=/usr/include/stdc-predef.h\n} module } }
