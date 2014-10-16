/* { dg-do compile } */
/* { dg-options "-w -fpermissive" } */

int strlen (const char *p);

int f (char *p)
{
    int x = strlen (p);
    return x;
}

