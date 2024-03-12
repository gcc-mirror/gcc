/* { dg-do compile } */
/* { dg-additional-options "-save-temps" } */
/* PR preprocessor/90400 */

#define OUTER(x) x
#define FOR(x) _Pragma ("GCC unroll 0") for (x)
void f ()
{
    /* If the pragma were to be seen prior to the expansion of FOR, as was
       the case before r12-5454, then the unroll pragma would complain
       because the immediately following statement would be ";" rather than
       a loop.  */
    OUTER (; FOR (int i = 0; i != 1; ++i);) /* { dg-bogus {statement expected before ';' token} } */
}
