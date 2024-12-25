/* { dg-do compile } */
/* { dg-options "-mcpu=ev4" } */

unsigned int ntfs_getinfo(void *p)
{
    char bootsect[8];

    __builtin_memcpy(bootsect, p, sizeof bootsect);
    return *(unsigned short *)(bootsect + 3);
}
