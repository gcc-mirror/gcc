/* PR middle-end/31309 */
/* Origin: Peeter Joot <peeterj@ca.ibm.com> */

/* { dg-do run { target *-*-linux* } } */

#include <sys/mman.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

unsigned long ossAlignX(unsigned long i, unsigned long X)
{
   return ((i + (X - 1)) & ~(unsigned long) (X - 1));
}

struct STRUCT_6_BYTES
{
   unsigned char slot[sizeof(unsigned short)];
   unsigned char page[sizeof(unsigned int)];
};

struct SQLU_DICT_INFO_0
{
   void *pBlah;
   char bSomeFlag1;
   char bSomeFlag2;
   struct STRUCT_6_BYTES dRID;
};

struct SQLU_DATAPART_0
{
   struct SQLU_DICT_INFO_0 *pDictRidderInfo;
};

struct XXX
{
   struct SQLU_DATAPART_0 *m_pDatapart;
};

struct STRUCT_6_BYTES INIT_6_BYTES_ZERO()
{
   struct STRUCT_6_BYTES ridOut = {{0,0}, {0,0,0,0}};
   return ridOut;
}

void Initialize(struct XXX *this, int iIndex)
{
   struct SQLU_DICT_INFO_0 *pDictRidderInfo
     = this->m_pDatapart[iIndex].pDictRidderInfo;
   pDictRidderInfo->bSomeFlag1 = 0;
   pDictRidderInfo->bSomeFlag2 = 0;
   pDictRidderInfo->dRID = INIT_6_BYTES_ZERO();
}

int main(void)
{
   int rc;

   struct stuff
   {
      char c0[4096-sizeof(struct XXX)];
      struct XXX o;
      char c1[4096*2-sizeof(struct SQLU_DATAPART_0)];
      struct SQLU_DATAPART_0 dp;
      char c2[4096*2-sizeof(struct SQLU_DICT_INFO_0)];
      struct SQLU_DICT_INFO_0 di;
      char c3[4096];
   };

   char buf[sizeof(struct stuff)+4096];
   struct stuff *u = (struct stuff *)ossAlignX((unsigned long)&buf[0], 4096);
   memset(u, 1, sizeof(struct stuff));
   u->c1[0] = '\xAA';
   u->c2[0] = '\xBB';
   u->c3[0] = '\xCC';

   rc = mprotect(u->c1, 4096, PROT_NONE);
   if (rc == -1)
      printf("mprotect:c1: %d: %d(%s)\n", rc, errno, strerror(errno));

   rc = mprotect(u->c2, 4096, PROT_NONE);
   if (rc == -1)
      printf("mprotect:c2: %d: %d(%s)\n", rc, errno, strerror(errno));

   rc = mprotect(u->c3, 4096, PROT_NONE);
   if (rc == -1)
      printf("mprotect:c3: %d: %d(%s)\n", rc, errno, strerror(errno));

   u->o.m_pDatapart = &u->dp;
   u->dp.pDictRidderInfo = &u->di;
   Initialize(&u->o, 0);

   mprotect(u->c1, 4096, PROT_READ|PROT_WRITE);
   mprotect(u->c2, 4096, PROT_READ|PROT_WRITE);
   mprotect(u->c3, 4096, PROT_READ|PROT_WRITE);

   return 0;
}
