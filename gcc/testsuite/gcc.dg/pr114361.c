/* PR c/114361 */
/* { dg-do compile } */
/* { dg-options "-std=gnu23 -g" } */

void f()
{
    typedef struct foo bar;
    typedef __typeof( ({ (struct foo { bar *x; }){ }; }) ) wuz;
    struct foo { wuz *x; };
}
