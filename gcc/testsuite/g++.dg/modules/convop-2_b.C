// PR c++/122381
// { dg-additional-options "-fmodules" }

import "convop-2_a.H";
template struct color_ref<int>;
