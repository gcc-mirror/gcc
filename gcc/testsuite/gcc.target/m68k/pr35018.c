/* { dg-do compile } */
/* { dg-options "-Os -mcpu=5249" } */

static inline void vect_add(int *x, int *y, int n)
{
    asm volatile ("nop;"
                : [n] "+d" (n), [x] "+a" (x), [y] "+a" (y)
                : : "%d0", "%d1", "%d2", "%d3", "%a0", "%a1", "%a2", "%a3",
                    "cc", "memory");
}

extern void vect_copy (int *, int *, int);

void vorbis_synthesis_blockin(int *blocksizes)
{
    int j, *pcm, *p;

    int n=blocksizes[*p]/2;
    int n0=blocksizes[0]/2;
    int n1=blocksizes[1]/2;

    for(j=0;j<*p;j++)
    {
        vect_add(p, pcm, n1);
        vect_add(pcm, p, n0);
        vect_add(p, pcm, n0);
        vect_add(p, pcm, n0);
        vect_copy(pcm, p, n);
    }
}

