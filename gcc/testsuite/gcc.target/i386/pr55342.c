/* PR rtl-optimization/55342 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "notb" } } */


void convert_image(unsigned char *in, unsigned char *out, int size) {
    int i;
    unsigned char * read = in,
     * write = out;
    for(i = 0; i < size; i++) {
        unsigned char r = *read++;
        unsigned char g = *read++;
        unsigned char b = *read++;
        unsigned char c, m, y, k, tmp;
        c = 255 - r;
        m = 255 - g;
        y = 255 - b;
	if (c < m)
	  k = ((c) > (y)?(y):(c));
	else
          k = ((m) > (y)?(y):(m));
        *write++ = c - k;
        *write++ = m - k;
        *write++ = y - k;
        *write++ = k;
    }
}
