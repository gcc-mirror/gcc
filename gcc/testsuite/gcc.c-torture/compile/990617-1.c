/* { dg-do assemble } */
/* { dg-require-effective-target ptr32plus } */

int main()
{
    do {
        long l;
        long *p = &l;
        
        *p = 0x0000000070000000L;
        p += 2;
        {
            unsigned int *addr = (unsigned int *)0x70000000;
            printf("%d, %d\n", addr[1], addr[0]);
        }
        
    } while (1);
}

