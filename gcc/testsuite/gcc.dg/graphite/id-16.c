/* { dg-additional-options "--param graphite-allow-codegen-errors=1" } */

int transformation[(2*19 - 1) * (2*19 - 1)][8];

const int transformation2[8][2][2] = {
  {{1, 0}, {0, 1}},
  {{0, 1}, {-1, 0}},
  {{-1, 0}, {0, -1}},
  {{0, -1}, {1, 0}},
  {{0, -1}, {-1, 0}},
  {{-1, 0}, {0, 1}},
  {{0, 1}, {1, 0}},
  {{1, 0}, {0, -1}}
};

void
transformation_init (void)
{
  int k;
  int dx;
  int dy;

  for (k = 0; k < 8; k++)
    {
      for (dy = -19 + 1; dy <= 19 - 1; dy++)
	{
	  for (dx = -19 + 1; dx <= 19 - 1; dx++)
	    {
	      int tx;
	      int ty;
	      do
		{
		  *&tx =
		    transformation2[k][0][0] * (dx) +
		    transformation2[k][0][1] * (dy);
		  *&ty =
		    transformation2[k][1][0] * (dx) +
		    transformation2[k][1][1] * (dy);
		}
	      while (0);
	      transformation[((dy + 19 - 1) * (2 * 19 - 1) +
			      (dx + 19 - 1))][k] = ((tx) * (19 + 1) + (ty));
	    }
	}
    }
}

/* { dg-final { scan-tree-dump-times "code generation error" 1 "graphite" } } */
