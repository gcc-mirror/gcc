extern void abort(void);

typedef struct myCDerived
{
   int cInt;
   double cDouble;
   float cFloat;
   short cShort;
   void *ptr;
}myCDerived_t;

#define DERIVED_ARRAY_LEN 10
#define ARRAY_LEN_2 3
#define DIM1 2
#define DIM2 3

void testDerivedPtrs(myCDerived_t *cDerivedPtr,
                     myCDerived_t *derivedArray, int arrayLen,
                     myCDerived_t *derived2d, int dim1, int dim2);

int main(int argc, char **argv)
{
   myCDerived_t cDerived;
   myCDerived_t derivedArray[DERIVED_ARRAY_LEN];
   myCDerived_t derived2DArray[DIM1][DIM2];
   int i = 0;
   int j = 0;

   cDerived.cInt = 1;
   cDerived.cDouble = 2.0;
   cDerived.cFloat = 3.0;
   cDerived.cShort = 4;
/*    cDerived.ptr = NULL; */
   /* nullify the ptr */
   cDerived.ptr = 0;

   for(i = 0; i < DERIVED_ARRAY_LEN; i++)
   {
      derivedArray[i].cInt = (i+1) * 1;
      derivedArray[i].cDouble = (i+1) * 1.0; /* 2.0; */
      derivedArray[i].cFloat = (i+1) * 1.0; /* 3.0; */
      derivedArray[i].cShort = (i+1) * 1; /* 4; */
/*       derivedArray[i].ptr = NULL; */
      derivedArray[i].ptr = 0;
   }

   for(i = 0; i < DIM1; i++)
   {
      for(j = 0; j < DIM2; j++)
      {
         derived2DArray[i][j].cInt = ((i*DIM1) * 1) + j;
         derived2DArray[i][j].cDouble = ((i*DIM1) * 1.0) + j;
         derived2DArray[i][j].cFloat = ((i*DIM1) * 1.0) + j;
         derived2DArray[i][j].cShort = ((i*DIM1) * 1) + j;
/*          derived2DArray[i][j].ptr = NULL; */
         derived2DArray[i][j].ptr = 0;
      }
   }

   /* send in the transpose size (dim2 is dim1, dim1 is dim2) */
   testDerivedPtrs(&cDerived, derivedArray, DERIVED_ARRAY_LEN,
                   derived2DArray[0], DIM2, DIM1);
   
   return 0;
}/* end main() */

