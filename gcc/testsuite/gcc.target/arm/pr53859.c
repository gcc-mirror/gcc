/* PR target/53859 */
/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-mcpu=cortex-m4 -mthumb -O2" } */

void bar (int,int,char* ,int);

void foo (char c)
{
    bar (1,2,&c,3);
}
