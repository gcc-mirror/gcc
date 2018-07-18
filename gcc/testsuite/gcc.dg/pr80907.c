/* { dg-do compile } */
/* { dg-options "-O3 -Warray-bounds" } */



int x[3];
int n=2;
void foo() 
{
    for(int i=0;i<n;i++) for(int j=0;j<=i;j++) x[i+j]++;
}

