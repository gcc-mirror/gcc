// Test that asm-qualifiers are not allowed on toplevel asm.
// { dg-do compile }
// { dg-options "-std=gnu++98" }

asm const ("");    // { dg-error {'const' is not an asm qualifier} }
asm volatile (""); // { dg-warning {asm qualifier 'volatile' ignored outside of function body} }
asm restrict (""); // { dg-error {expected '\(' before 'restrict'} }
asm inline ("");   // { dg-error {asm qualifier outside of function body} }
asm goto ("");     // { dg-error {asm qualifier outside of function body} }

// There are many other things wrong with this code, so:
// { dg-excess-errors "" }
