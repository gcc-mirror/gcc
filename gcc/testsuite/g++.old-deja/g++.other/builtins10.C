// Build don't link:
// Test that built-in functions don't warn when prototyped without arguments.
// Origin: PR c++/9367
// Copyright (C) 2003 Free Software Foundation.

extern "C" int snprintf();

