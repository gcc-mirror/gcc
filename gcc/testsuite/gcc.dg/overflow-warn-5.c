/* PR c/27273 */
/* { dg-do compile } */
/* { dg-options "-Woverflow" } */

unsigned char rx_async(unsigned char p) {
    return p & 512; /* { dg-warning "overflow in conversion from .int. to .unsigned char. changes value" } */
}
