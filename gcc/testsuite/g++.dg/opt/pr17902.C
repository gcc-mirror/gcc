/* { dg-options "-O3" } */
/* { dg-do compile } */

void foo();
struct A { ~A(){ foo(); } };
struct B { A a; };
void bar()
{
    A a;
    bool b = false;
    int i, j;


    for (j=0; j<i; j++)
    {
        if (i) b=true;
        if (j && i) foo();
        if (j && i) i++;
    }


    for (j=0; j<i; j++)
        if ( !j || (j==1 && b && i) )
            B x;
}

