/* { dg-do compile { target { ! { nvptx*-*-* visium-*-* } } } } */
/* { dg-additional-options "-fno-pie" { target sparc*-*-* } } */

void
 __attribute__((patchable_function_entry(65536)))
foo1 (void) { /* { dg-warning "'patchable_function_entry' attribute argument '65536' is out of range" } */
}

void
 __attribute__((patchable_function_entry(65536,1)))
foo2 (void) { /* { dg-warning "'patchable_function_entry' attribute argument '65536' is out of range" } */
}

void
 __attribute__((patchable_function_entry(65536,65536)))
foo3 (void) { /* { dg-warning "'patchable_function_entry' attribute argument '65536' is out of range" } */
}
