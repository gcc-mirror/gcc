// { dg-do compile { target c++20 } }
// { dg-additional-options "-fmodules-ts" }

import using_enum_3;

static_assert(text_encoding::id::CP50220 == text_encoding::CP50220);
