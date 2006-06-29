/* { dg-do compile } */
/* { dg-options "-O2" } */

void foo()
{
    goto L;
    if (0..) { L: ; }  // { dg-error "too many decimal points" }
}

