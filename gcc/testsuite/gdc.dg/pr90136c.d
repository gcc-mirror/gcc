// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=93038
// { dg-do compile }
// { dg-options "-Wattributes" }

import gcc.attribute;

@attribute("forceinline") int sum_array(int[] input);
@attribute("noinline") int sum_array(int[] input);
// { dg-warning "ignoring attribute .noinline. because it conflicts with attribute .forceinline." "" { target *-*-* } .-1 }
