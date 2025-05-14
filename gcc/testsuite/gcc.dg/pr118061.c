/* { dg-do "compile" } */
/* { dg-options "-std=gnu23" } */

int main()
{
    struct { int x[1++]; } x;	/* { dg-error "lvalue required as increment operand" } */
    struct { int x[1++]; } y;	/* { dg-error "lvalue required as increment operand" } */
}
