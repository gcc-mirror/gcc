/* { dg-do compile } */
/* { dg-options "-fdump-rtl-expand-all" */
/* { dg-final { cleanup-rtl-dump "expand" } } */
int isdigit(int c)
{
        return c >= 0;
}


