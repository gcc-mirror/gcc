// { dg-options "-O1" }

#include <stdio.h>
struct A {
     A(int arg) : ia(arg) {}
     int x,y,z,ia;
     int mf(int arg) { return arg + ia; }
};
int func(int A::*pdm, int (A::*pmf)(int)) //      2.      regular function
{ 
     A oa(2);
     return ((&oa)->*pdm) + (oa.*pmf)(2); 
}       
int main()
{
     int val;

     int A::*pda = &A::ia;           
     int (A::*pmfa)(int) = &A::mf;   
     val = func( pda, pmfa );
     if(val != 6)
       printf("val=%d, expect 6 \n", val);
}
