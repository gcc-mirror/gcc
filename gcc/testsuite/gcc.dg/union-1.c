/* PR target/15783 */
/* Origin: Paul Pluzhnikov <ppluzhnikov@charter.net> */

/* This used to ICE on SPARC 64-bit because the back-end was
   returning an invalid construct for the return value of fu2.  */

/* { dg-do compile } */

union u2 {
    struct
    {
        int u2s_a, u2s_b, u2s_c, u2s_d, u2s_e;
    } u2_s;
    double u2_d;
} u2a;

union u2 fu2();

void unions()
{
    u2a = fu2();
}
