/* { dg-do compile } */
/* { dg-options "-O1 -ftree-vectorize" } */

void f(unsigned char *mem)
{
   int i;

   for(i=0;i<4;i++) {
     while(mem[i]==0) ;
   }
}
