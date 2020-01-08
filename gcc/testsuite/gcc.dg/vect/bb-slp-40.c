/* { dg-do compile } */
/* { dg-additional-options "-fvect-cost-model=dynamic" } */
/* { dg-require-effective-target vect_char_add } */

char g_d[1024], g_s1[1024], g_s2[1024];
void foo(void)
{
    char *d = g_d, *s1 = g_s1, *s2 = g_s2;

    for ( int y = 0; y < 128; y++ )
    {
      d[0 ] = s1[0 ] + s2[0 ];
      d[1 ] = s1[1 ] + s2[1 ];
      d[2 ] = s1[2 ] + s2[2 ];
      d[3 ] = s1[3 ] + s2[3 ];
      d[4 ] = s1[4 ] + s2[4 ];
      d[5 ] = s1[5 ] + s2[5 ];
      d[6 ] = s1[6 ] + s2[6 ];
      d[7 ] = s1[7 ] + s2[7 ];
      d[8 ] = s1[8 ] + s2[8 ];
      d[9 ] = s1[9 ] + s2[9 ];
      d[10] = s1[10] + s2[10];
      d[11] = s1[11] + s2[11];
      d[12] = s1[12] + s2[12];
      d[13] = s1[13] + s2[13];
      d[14] = s1[14] + s2[14];
      d[15] = s1[15] + s2[15];
      d += 16;
    }
}

/* See that we vectorize an SLP instance.  */
/* { dg-final { scan-tree-dump "Analyzing vectorizable constructor" "slp1" } } */
/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "slp1" } } */
