/* PR target/103020 */
/* { dg-do compile { target { ! int128 } } } */
/* { dg-additional-options "-mavx512f" } */

typedef int TI __attribute__((mode (TI)));	/* { dg-error "unable to emulate" } */
typedef int V1TI __attribute__((mode (V1TI)));	/* { dg-error "unable to emulate" } */
typedef int V2TI __attribute__((mode (V2TI)));	/* { dg-error "unable to emulate" } */
typedef int V4TI __attribute__((mode (V4TI)));	/* { dg-error "unable to emulate" } */
/* { dg-warning "is deprecated" "V1TI" { target *-*-* } .-3 } */
/* { dg-warning "is deprecated" "V2TI" { target *-*-* } .-3 } */
/* { dg-warning "is deprecated" "V4TI" { target *-*-* } .-3 } */
