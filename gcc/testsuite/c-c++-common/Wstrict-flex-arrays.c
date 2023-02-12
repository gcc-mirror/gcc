/* Test the usage of option -Wstrict-flex-arrays.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-flex-arrays" } */

int main(int argc, char *argv[])
{
    return 0;
}
/* { dg-warning "is ignored when \'-fstrict-flex-arrays\' is not present" "" { target *-*-* } 0 } */
