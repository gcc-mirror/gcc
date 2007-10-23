/* { dg-do compile } */
/* { dg-options "-O3 -ftree-vectorize" } */

/* Testcase by Martin Michlmayr <tbm@cyrius.com> */

extern int sscanf (__const char *__restrict __s,
                  __const char *__restrict __format, ...);
unsigned char got_elevation_pattern;
struct site
{
  float antenna_pattern[361][1001];
}
LR;
void
LoadPAT (char *filename)
{
  int x, y;
  char string[255];
  float elevation, amplitude, elevation_pattern[361][1001];
  for (x = 0; filename[x] != '.' ; x++)
    sscanf (string, "%f %f", &elevation, &amplitude);
  for (y = 0; y <= 1000; y++)
  {
    if (got_elevation_pattern)
      elevation = elevation_pattern[x][y];
    else
      elevation = 1.0;
    LR.antenna_pattern[x][y] = elevation;
  }
}

/* { dg-final { cleanup-tree-dump "vect" } } */
