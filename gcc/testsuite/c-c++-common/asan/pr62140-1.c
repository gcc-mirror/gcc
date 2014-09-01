/* { dg-do compile } */
/* { dg-options "-w -fpermissive" } */

int memcmp (const void *p, const void *q, int len);

int f (int *p, int *q, int len)
{
    return memcmp (p, q, len);
}

