/* { dg-do compile } */

/* ICEd with type-checking enabled.  */

unsigned int mgaSetTexImages(int i)
{
    return ((i | 0x40) & 0xffffffc0);
}
