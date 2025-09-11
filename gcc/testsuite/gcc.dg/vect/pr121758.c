/* { dg-do compile } */

long g_2205, g_3005;
int g_3320;
void main()
{
  for (; g_2205; g_2205 += 1)
    {
      g_3005 = 0;
      for (; g_3005 <= 8; g_3005 += 1)
	g_3320 &= 611 & (unsigned char)g_3005;
    }
}

/* { dg-final { scan-tree-dump-not "failed to update reduction index" "vect" } } */
