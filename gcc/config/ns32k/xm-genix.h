/* Config file for ns32k running system V.  */

#define memcpy(src,dst,len) bcopy ((dst),(src),(len))
#define memset gcc_memset
#define memcmp(left,right,len) bcmp ((left),(right),(len))

#define USG
