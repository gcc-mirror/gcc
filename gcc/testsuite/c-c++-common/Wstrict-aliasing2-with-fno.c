/* Test the usage of option -Wstrict-aliasing.  */
/* Make sure it's enabled even when -fno-strict-aliasing.  */
/* Set -Wstrict-aliasing=2 so it warns on casts */
/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing=2 -fno-strict-aliasing" } */

int main(int argc, char *argv[])
{
    int x;
    float *q = (float*) &x; /* { dg-warning "strict-aliasing" } */
    return x;
}
