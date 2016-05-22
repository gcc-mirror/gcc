
/* { dg-do compile } */
/* { dg-options "-O2  -fdump-tree-optimized -ffast-math" } */

unsigned int foo (unsigned int x, unsigned int y, unsigned int z)
{
      return x + (-y * z * z);
}

float bar (float x, float y, float z)
{
      return x + (-y * z * z);
}

float bar2 (float x, float y, float z)
{
      return x + (-y * z * z * 5.0f);
}

float bar3 (float x, float y, float z)
{
      return x + (-y * x * -z);
}


/* { dg-final { scan-tree-dump-times "_* = -y_" 0 "optimized" } } */
