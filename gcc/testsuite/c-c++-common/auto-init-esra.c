/* Verify the strength reduction adjustment for -ftrivial-auto-var-init.  */ 
/* { dg-do compile } */
/* { dg-options "-O2 -ftrivial-auto-var-init=zero -fdump-tree-gimple -fdump-tree-esra" } */


typedef double VECTOR[3];

enum
{
 X = 0,
 Y = 1,
 Z = 2,
 T = 3
};

void Assign_Vector(VECTOR d, VECTOR s)
{
 d[X] = s[X];
 d[Y] = s[Y];
 d[Z] = s[Z];
}

void VCross(VECTOR a, const VECTOR b, const VECTOR c)
{
 VECTOR tmp;

 tmp[X] = b[Y] * c[Z] - b[Z] * c[Y];
 tmp[Y] = b[Z] * c[X] - b[X] * c[Z];
 tmp[Z] = b[X] * c[Y] - b[Y] * c[X];

 Assign_Vector(a, tmp);
}

/* { dg-final { scan-tree-dump-times "tmp = .DEFERRED_INIT \\(24, 2, 0\\)" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times ".DEFERRED_INIT \\(8, 2, 0\\)" 3 "esra" } } */
