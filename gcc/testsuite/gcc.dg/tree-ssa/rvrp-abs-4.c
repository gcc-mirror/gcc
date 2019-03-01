/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-rvrp" } */

void
foo (distance, i, j)
     int distance[13][13];
     int i, j;
{
 if (distance[i][j] < 0)
   distance[i][0] = ((distance[i][j]) < 0 ? -(distance[i][j])  : (distance[i][j]));
}

void
foo2 (distance, i, j)
     int distance[13][13];
     int i, j;
{
 if (distance[i][j] <= 0)
   distance[i][0] = ((distance[i][j]) < 0 ? -(distance[i][j])  : (distance[i][j]));
}

void
foo3 (distance, i, j)
     int distance[13][13];
     int i, j;
{
 if (distance[i][j] > 0)
   distance[i][0] = ((distance[i][j]) < 0 ? -(distance[i][j])  : (distance[i][j]));
}

void
foo4 (distance, i, j)
     double distance[13][13];
     int i, j;
{
 if (distance[i][j] >= 0)
   distance[i][0] = ((distance[i][j]) < 0 ? -(distance[i][j])  : (distance[i][j]));
}

/* { dg-final { scan-tree-dump-times "ABS_EXPR " 0 "rvrp"} } */
