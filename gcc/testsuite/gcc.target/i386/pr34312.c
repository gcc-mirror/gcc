/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -march=pentium-m -fpic" } */

typedef struct
{
  unsigned char seq[3];
} JamoNormMap;

static const JamoNormMap *
JamoClusterSearch (JamoNormMap aKey, const JamoNormMap * aClusters,
		   short aClustersSize)
{
  unsigned short l = 0, u = aClustersSize - 1;
  unsigned short h = (l + u) / 2;

  if ((aKey.seq[1] - aClusters[h].seq[1]) < 0)
    return JamoClusterSearch (aKey, &(aClusters[l]), h - l);
}

short
JamoSrchReplace (const JamoNormMap * aClusters, unsigned short aClustersSize,
		 unsigned short * aIn, unsigned int * aLength,
		 unsigned short aOffset)
{
  JamoNormMap key;

  key.seq[0] = 0;
  key.seq[1] = 1;
  key.seq[2] = 2;

  JamoClusterSearch (key, aClusters, aClustersSize);
}
