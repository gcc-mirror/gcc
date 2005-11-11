extern int unknown;
extern int n0;
extern short *s0;
extern int n1;
extern short *s1;
extern short *s2;
extern int *n2;
extern int *n3;
extern int n4;
extern short *n5;
extern int *n6;
extern int n7;
extern char *unc;

void
f (short *sp)
{
  int j = 0;
  int i = 0;
  int n8 = 0;
  int n9 = 0;
  short *s3 = sp;
  short s4 = (short) unknown;
  short s5 = (short) unknown;
  char *c0 = unc;
  int n10 = 0;
  int n11 = 0;
  int u0 = unknown;
  int k = 0;

  for (n8 = 1; n8 <= n7; n8++)
    {
      for (i = 1; i <= n0; i++)
	c0[i] = 0;
      for (i = 1; i <= u0; i++) { }
      for (i = sp[0]; i != -32767; i = sp[i])
	if (s4 == u0)
	  for (j = 1; j <= u0; j++)
	    if (!c0[s3[j]])
	      break;
    }
  for (n9 = 1; n9 <= n0; n9++) s1[unknown + n9] = n9;
  for (i = 1; i <= n1; i++)
    for (j = 1; j <= s4; j++)
      s3[j] = s1[s3[j]];
  for (n8 = 1; n8 <= n7; n8++)
    for (i = 1; i <= s5; i++)
      s3[i] = s1[s3[i]];
  for (n9 = 1; n9 <= n0; n9++) sp[s1[n9]] = unknown;
  for (n10 = 2; n10 < n4; n10++) { }
  for (k = 1; k <= unknown; k++)
    {
      s4 = s0[n5[u0]];
      for (i = 1; i <= s4; i++) { }
      for (j = 1; j <= s4; j++)
	if (n2[1] != 0)
	  if (i == unknown)
	    unknown = n3[unknown];
      n6[u0] = n10;
    }
  for (k = n7; k >= n11; k--)
    if (n2[k] == -32767)
      break;
  free (c0);

  for (i = 1; i <= n7; i++)
    {
      for (j = 1; j <= s4; j++) { }
      for (n8 = s2[unknown]; n8 != -32767; n8 = s2[n8]) { }
      for (j = 1; j <= s5; j++) { }
    }
}
