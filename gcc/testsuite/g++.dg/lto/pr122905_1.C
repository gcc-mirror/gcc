// { dg-options "-O2 -flto -std=c++17" }

#include "pr122905.h"

volatile auto v = foo ();
