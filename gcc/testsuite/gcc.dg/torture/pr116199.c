/* { dg-do compile } */
/* { dg-require-effective-target int32 } */

__extension__ typedef unsigned long long int __uint64_t;
__extension__ typedef __uint64_t __dev_t;

__dev_t __gnu_dev_makedev (unsigned int __major, unsigned int __minor)
{
  __dev_t __dev;
  __dev = (((__dev_t) (__major & 0x00000fffu)) << 8);
  __dev |= (((__dev_t) (__major & 0xfffff000u)) << 32);
  return __dev;
}
