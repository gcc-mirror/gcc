/* PR target/53859 */
/* { dg-do compile } */
/* { dg-require-effective-target arm_cpu_cortex_m4_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_cpu_cortex_m4 } */

void bar (int,int,char* ,int);

void foo (char c)
{
    bar (1,2,&c,3);
}
