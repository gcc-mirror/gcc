/* Test the usage of option -Wstrict-aliasing.  */
/* Make sure it's enabled even when -fno-strict-aliasing.  */
/* Set -Wstrict-aliasing=3 so that it only warns on dereference */
/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing=3 -fno-strict-aliasing" } */

int main(int argc, char *argv[])
{
    int x;
    *(float*) &x = 42; /* { dg-warning "strict-aliasing" } */
    return x;
}
