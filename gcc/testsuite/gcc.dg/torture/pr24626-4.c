/* { dg-do compile } */
/* { dg-options "-fmodulo-sched" } */

int foo(short* vec1, short* vec2, short* vec3,int len )
{
        int temp,i;
        for (i=0; i<len; i++) {
                 temp = vec1[i] * 2;
                 temp += vec2[i] * 3 ;
                 vec3[i] = temp;
        }
}
