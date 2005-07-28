/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

float x[3];

void foo()
{
    int i;

    for (i=0; i<5; ++i) x[i]=0;
    for (i=0; i<4; ++i) ;
}
