// PR c++/100881
// { dg-additional-options "-std=c++20 -fmodules-ts" }
import pr100881;

static_assert(current_line_fn() == 5);
static_assert(current_line_cls{}.line == 6);
static_assert(current_line_fn_tmpl<int>() == 7);
static_assert(current_line_cls_tmpl<int>{}.line == 8);
