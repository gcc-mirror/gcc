/* PR c/27273 */
/* { dg-do compile } */

// This used to warn about "overflow in implicit constant conversion",
// which was wrong; 512 is never converted to unsigned char.  Rather, an
// appropriate warning would be that the & expression always evaluates to 0
// because of the limited range of unsigned char.

unsigned char rx_async(unsigned char p) {
    return p & 512; /* { dg-warning "" "" { xfail *-*-* } } */
}
