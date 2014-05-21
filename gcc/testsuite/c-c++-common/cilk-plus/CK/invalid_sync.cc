/* PR c/60189 */
/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int main (void)
{
    _Cilk_sync return; /* { dg-error " expected ';' before 'return'" } */
    return 0;
}
