/* PR 3967

   local-alloc screwed up consideration of high+lo_sum and created
   reg_equivs that it shouldn't have, resulting in lo_sum with
   uninitialized data, resulting in segv.  The test has to remain
   relatively large, since register spilling is required to twig
   the bug.  */

unsigned long *Local1;
unsigned long *Local2;
unsigned long *Local3;
unsigned long *RDbf1;
unsigned long *RDbf2;
unsigned long *RDbf3;
unsigned long *IntVc1;
unsigned long *IntVc2;
unsigned long *IntCode3;
unsigned long *IntCode4;
unsigned long *IntCode5;
unsigned long *IntCode6;
unsigned long *Lom1;
unsigned long *Lom2;
unsigned long *Lom3;
unsigned long *Lom4;
unsigned long *Lom5;
unsigned long *Lom6;
unsigned long *Lom7;
unsigned long *Lom8;
unsigned long *Lom9;
unsigned long *Lom10;
unsigned long *RDbf11;
unsigned long *RDbf12;

typedef struct
  {
    long a1;
    unsigned long n1;
    unsigned long local1;
    unsigned long local2;
    unsigned long local3;
    unsigned long rdbf1;
    unsigned long rdbf2;
    unsigned long milli;
    unsigned long frames1;
    unsigned long frames2;
    unsigned long nonShared;
    long newPrivate;
    long freeLimit;
    unsigned long cache1;
    unsigned long cache2;
    unsigned long cache3;
    unsigned long cache4;
    unsigned long cache5;
    unsigned long time6;
    unsigned long frames7;
    unsigned long page8;
    unsigned long ot9;
    unsigned long data10;
    unsigned long bm11;
    unsigned long misc12;
  }
ShrPcCommonStatSType;


typedef struct
  {
    unsigned long sharedAttached;
    unsigned long totalAttached;
    long avgPercentShared;
    unsigned long numberOfFreeFrames;
    unsigned long localDirtyPageCount;
    unsigned long globalDirtyPageCount;
    long wakeupInterval;
    unsigned long numActiveProcesses;
    unsigned long numRecentActiveProcesses;
    unsigned long gemDirtyPageKinds[10];
    unsigned long stoneDirtyPageKinds[10];
    unsigned long gemsInCacheCount;
    long targetFreeFrameCount;
  }
ShrPcMonStatSType;

typedef struct
  {
    unsigned long c1;
    unsigned long c2;
    unsigned long c3;
    unsigned long c4;
    unsigned long c5;
    unsigned long c6;
    unsigned long c7;
    unsigned long c8;
    unsigned long c9;
    unsigned long c10;
    unsigned long c11;
    unsigned long c12;
    unsigned long a1;
    unsigned long a2;
    unsigned long a3;
    unsigned long a4;
    unsigned long a5;
    unsigned long a6;
    unsigned long a7;
    unsigned long a8;
    unsigned long a9;
    unsigned long a10;
    unsigned long a11;
    unsigned long a12;
    unsigned long a13;
    unsigned long a14;
    unsigned long a15;
    unsigned long a16;
    unsigned long a17;
    unsigned long a18;
    unsigned long a19;
    unsigned long sessionStats[40];
  }
ShrPcGemStatSType;

union ShrPcStatUnion
  {
    ShrPcMonStatSType monitor;
    ShrPcGemStatSType gem;
  };

typedef struct
  {
    int processId;
    int sessionId;
    ShrPcCommonStatSType cmn;
    union ShrPcStatUnion u;
  } ShrPcStatsSType;

typedef struct
  {
    unsigned long *p1;
    unsigned long *p2;
    unsigned long *p3;
    unsigned long *p4;
    unsigned long *p5;
    unsigned long *p6;
    unsigned long *p7;
    unsigned long *p8;
    unsigned long *p9;
    unsigned long *p10;
    unsigned long *p11;
  }
WorkEntrySType;

WorkEntrySType Workspace;

static void 
setStatPointers (ShrPcStatsSType * statsPtr, long sessionId)
{
  statsPtr->sessionId = sessionId;
  statsPtr->cmn.a1 = 0;
  statsPtr->cmn.n1 = 5;

  Local1 = &statsPtr->cmn.local1;
  Local2 = &statsPtr->cmn.local2;
  Local3 = &statsPtr->cmn.local3;
  RDbf1 = &statsPtr->cmn.rdbf1;
  RDbf2 = &statsPtr->cmn.rdbf2;
  RDbf3 = &statsPtr->cmn.milli;
  *RDbf3 = 1;

  IntVc1 = &statsPtr->u.gem.a1;
  IntVc2 = &statsPtr->u.gem.a2;
  IntCode3 = &statsPtr->u.gem.a3;
  IntCode4 = &statsPtr->u.gem.a4;
  IntCode5 = &statsPtr->u.gem.a5;
  IntCode6 = &statsPtr->u.gem.a6;

  {
    WorkEntrySType *workSpPtr;
    workSpPtr = &Workspace;
    workSpPtr->p1 = &statsPtr->u.gem.a7;
    workSpPtr->p2 = &statsPtr->u.gem.a8;
    workSpPtr->p3 = &statsPtr->u.gem.a9;
    workSpPtr->p4 = &statsPtr->u.gem.a10;
    workSpPtr->p5 = &statsPtr->u.gem.a11;
    workSpPtr->p6 = &statsPtr->u.gem.a12;
    workSpPtr->p7 = &statsPtr->u.gem.a13;
    workSpPtr->p8 = &statsPtr->u.gem.a14;
    workSpPtr->p9 = &statsPtr->u.gem.a15;
    workSpPtr->p10 = &statsPtr->u.gem.a16;
    workSpPtr->p11 = &statsPtr->u.gem.a17;
  }
  Lom1 = &statsPtr->u.gem.c1;
  Lom2 = &statsPtr->u.gem.c2;
  Lom3 = &statsPtr->u.gem.c3;
  Lom4 = &statsPtr->u.gem.c4;
  Lom5 = &statsPtr->u.gem.c5;
  Lom6 = &statsPtr->u.gem.c6;
  Lom7 = &statsPtr->u.gem.c7;
  Lom8 = &statsPtr->u.gem.c8;
  Lom9 = &statsPtr->u.gem.c9;
  Lom10 = &statsPtr->u.gem.c10;
  RDbf11 = &statsPtr->u.gem.c11;
  RDbf12 = &statsPtr->u.gem.c12;
}

typedef struct
{
  ShrPcStatsSType stats;
} ShrPcPteSType;

ShrPcPteSType MyPte;

static void 
initPte (void *shrpcPtr, long sessionId)
{
  ShrPcPteSType *ptePtr;

  ptePtr = &MyPte;
  setStatPointers (&ptePtr->stats, sessionId);
}

void 
InitCache (int sessionId)
{
  initPte (0, sessionId);
}

int 
main (int argc, char *argv[])
{
  InitCache (5);
  return 0;
}
