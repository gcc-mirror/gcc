// PR rtl-optimization/88018
// { dg-do compile }
// { dg-skip-if "" { *-*-* }  { "*" } { "-O0" } }
// { dg-options "-fsanitize=thread -fno-ipa-pure-const -O1 -fno-inline-functions-called-once -w" }

#include "../pr69667.C"
