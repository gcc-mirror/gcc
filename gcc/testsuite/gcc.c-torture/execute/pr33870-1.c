extern void abort (void);

typedef struct PgHdr PgHdr;
typedef unsigned char u8;
struct PgHdr {
int y;
struct {
 unsigned int pgno;
 PgHdr *pNextHash, *pPrevHash;
 PgHdr *pNextFree, *pPrevFree;
 PgHdr *pNextAll;
 u8 inJournal;
 short int nRef;
 PgHdr *pDirty, *pPrevDirty;
 unsigned int notUsed;
} x;
};
PgHdr **xx;
volatile int vx;
static inline PgHdr *merge_pagelist(PgHdr *pA, PgHdr *pB)
{
 PgHdr result;
 PgHdr *pTail;
 xx = &result.x.pDirty;
 pTail = &result;
 while( pA && pB ){
   if( pA->x.pgno<pB->x.pgno ){
     pTail->x.pDirty = pA;
     pTail = pA;
     pA = pA->x.pDirty;
   }else{
     pTail->x.pDirty = pB;
     pTail = pB;
     pB = pB->x.pDirty;
   }
   vx = (*xx)->y;
 }
 if( pA ){
   pTail->x.pDirty = pA;
 }else if( pB ){
   pTail->x.pDirty = pB;
 }else{
   pTail->x.pDirty = 0;
 }
 return result.x.pDirty;
}

PgHdr * __attribute__((noinline)) sort_pagelist(PgHdr *pIn)
{
 PgHdr *a[25], *p;
 int i;
 __builtin_memset (a, 0, sizeof (a));
 while( pIn ){
   p = pIn;
   pIn = p->x.pDirty;
   p->x.pDirty = 0;
   for(i=0; i<25 -1; i++){
     if( a[i]==0 ){
       a[i] = p;
       break;
     }else{
       p = merge_pagelist(a[i], p);
       a[i] = 0;
       a[i] = 0;
     }
   }
   if( i==25 -1 ){
     a[i] = merge_pagelist(a[i], p);
   }
 }
 p = a[0];
 for(i=1; i<25; i++){
   p = merge_pagelist (p, a[i]);
 }
 return p;
}

int main()
{
 PgHdr a[5];
 PgHdr *p;
 a[0].x.pgno = 5;
 a[0].x.pDirty = &a[1];
 a[1].x.pgno = 4;
 a[1].x.pDirty = &a[2];
 a[2].x.pgno = 1;
 a[2].x.pDirty = &a[3];
 a[3].x.pgno = 3;
 a[3].x.pDirty = 0;
 p = sort_pagelist (&a[0]);
 if (p->x.pDirty == p)
   abort ();
 return 0;
}
