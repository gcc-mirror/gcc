// PR c++/127940
// { dg-additional-options "-fmodules -nostdinc++ -I[srcdir]/dummy-stdlib" }

#include <stdexcept>		// { dg-bogus {bits/stdc} }
