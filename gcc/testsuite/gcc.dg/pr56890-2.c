/* PR target/56890 */
/* Reported by Rainer Jung <rainer.jung@kippdata.de> */

/* { dg-do assemble } */
/* { dg-options "-O" } */

unsigned int buggy(unsigned int min, unsigned int max)
{
    unsigned int number;
    if (max < 16384) {
        unsigned short num16;
        num16 = min + (long) ((double) (max - min + 1.0) * (num16 / (65535 + 1.0)));
        return num16;
    }
    else {
        (number) = min + (long) ((double) (max - min + 1.0) * (number / (4294967295U + 1.0)));
    }
    return number;
}
