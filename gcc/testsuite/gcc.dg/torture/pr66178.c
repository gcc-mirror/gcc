/* { dg-do compile } */
/* { dg-require-effective-target label_values } */

int test(void)
{
    static int a =  ((char *)&&l1-(char *)&&l2)-1;
l1:
l2:
    return a;
}

int test2(void)
{
    static int a =  ((char *)&&l2-(char *)&&l3)+((char *)&&l1-(char *)&&l2);
l1:
l2:
l3:
    return a;
}
