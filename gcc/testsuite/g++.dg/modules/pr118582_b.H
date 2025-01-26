// PR c++/118582
// { dg-additional-options "-fmodule-header -fno-module-lazy -std=c++20" }
// { dg-module-cmi {} }

import "pr118582_a.H";
using T = I<R<int>>::type;
