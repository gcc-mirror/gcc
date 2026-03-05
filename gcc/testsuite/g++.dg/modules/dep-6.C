// PR c++/123622
// { dg-additional-options "-fmodules -M -MG -MF dep-6.d" }

import "dep-6.h"; // Must not exist!

// { dg-final { scan-file dep-6.d {\nCXX_IMPORTS \+= \./dep-6\.h\.c\+\+-module} } }
