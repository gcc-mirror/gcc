/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -fdump-tree-optimized" } */
float foo (float x)
{
 int i;
 float j;
 float a = 0;
 for (i = 0; i < 4; ++i)
   {
     for (j = 0; j < 4; ++j)
       {
         a += 1;
         x += a;
       }
   }
 return x;
}

/* { dg-final { scan-tree-dump-not "REDUC_PLUS" "optimized"} } */
