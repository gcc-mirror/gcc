// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=93038
// { dg-do compile }
// { dg-options "-Wattributes" }

import gcc.attributes;

@attribute("always_inline") int sum_array(int[] input);
@attribute("noinline") int sum_array(int[] input);
// { dg-warning "ignoring attribute .noinline. because it conflicts with attribute .always_inline." "" { target *-*-* } .-1 }
