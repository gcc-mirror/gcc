/* PR c/10083 */
/* This will result in a very small constant for umul_highpart, which
   uncovered a bug in the Alpha machine description.  */

unsigned long f(unsigned long x) {
    return x % 0x3fffffffffffffff;
}
