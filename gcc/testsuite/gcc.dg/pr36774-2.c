/* Nested functions shouldn't produce warnings if defined before first use.
   Bug 36774. Test with -Wmissing-declarations. */
/* { dg-do compile } */
/* { dg-options "-Wmissing-declarations" } */

int foo(int a) { /* { dg-warning "no previous declaration" } */
    int bar(int b) { return b; } /* { dg-bogus "no previous declaration" } */
    return bar(a);
}
