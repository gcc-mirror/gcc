/* Nested functions shouldn't produce warnings if defined before first use.
   Bug 36774. Test with -Wmissing-prototypes. */
/* { dg-do compile } */
/* { dg-options "-Wmissing-prototypes" } */

int foo(int a) { /* { dg-warning "no previous prototype" } */
    int bar(int b) { return b; } /* { dg-bogus "no previous prototype" } */
    return bar(a);
}
