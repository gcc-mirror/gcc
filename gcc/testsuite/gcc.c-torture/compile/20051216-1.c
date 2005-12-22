/* PR rtl-optimization/25432 */

void *malloc (__SIZE_TYPE__);
void *realloc (void *, __SIZE_TYPE__);

struct A { double x, y; };
struct B { double x0, y0, x1, y1; };
struct C { int n_points; int dir; struct B bbox; struct A *points; };
struct D { int n_segs; struct C segs[1]; };

void foo (int, int, int *, int, int *, struct A **, int *, int *,
	  struct D *, int *, struct D **, int *, int **);
int baz (struct A, struct A, struct A, struct A);

static void
bar (struct D *svp, int *n_points_max,
     struct A p, int *seg_map, int *active_segs, int i)
{
  int asi, n_points;
  struct C *seg;

  asi = seg_map[active_segs[i]];
  seg = &svp->segs[asi];
  n_points = seg->n_points;
  seg->points = ((struct A *)
		realloc (seg->points, (n_points_max[asi] <<= 1) * sizeof (struct A)));
  seg->points[n_points] = p;
  seg->bbox.y1 = p.y;
  seg->n_points++;
}

struct D *
test (struct D *vp)
{
  int *active_segs, n_active_segs, *cursor, seg_idx;
  double y, share_x;
  int tmp1, tmp2, asi, i, j, *n_ips, *n_ips_max, n_segs_max;
  struct A **ips, p_curs, *pts;
  struct D *new_vp;
  int *n_points_max, *seg_map, first_share;

  n_segs_max = 16;
  new_vp = (struct D *) malloc (sizeof (struct D) +
				(n_segs_max - 1) * sizeof (struct C));
  new_vp->n_segs = 0;

  if (vp->n_segs == 0)
    return new_vp;

  active_segs = ((int *) malloc ((vp->n_segs) * sizeof (int)));
  cursor = ((int *) malloc ((vp->n_segs) * sizeof (int)));

  seg_map = ((int *) malloc ((vp->n_segs) * sizeof (int)));
  n_ips = ((int *) malloc ((vp->n_segs) * sizeof (int)));
  n_ips_max = ((int *) malloc ((vp->n_segs) * sizeof (int)));
  ips = ((struct A * *) malloc ((vp->n_segs) * sizeof (struct A *)));

  n_points_max = ((int *) malloc ((n_segs_max) * sizeof (int)));

  n_active_segs = 0;
  seg_idx = 0;
  y = vp->segs[0].points[0].y;
  while (seg_idx < vp->n_segs || n_active_segs > 0)
    {
      for (i = 0; i < n_active_segs; i++)
	{
	  asi = active_segs[i];
	  if (vp->segs[asi].n_points - 1 == cursor[asi] &&
	      vp->segs[asi].points[cursor[asi]].y == y)
	    i--;
	}

      while (seg_idx < vp->n_segs && y == vp->segs[seg_idx].points[0].y)
	{
	  cursor[seg_idx] = 0;
	  n_ips[seg_idx] = 1;
	  n_ips_max[seg_idx] = 2;
	  ips[seg_idx] =
	    ((struct A *) malloc ((n_ips_max[seg_idx]) * sizeof (struct A)));
	  ips[seg_idx][0] = vp->segs[seg_idx].points[0];
	  pts = ((struct A *) malloc ((16) * sizeof (struct A)));
	  pts[0] = vp->segs[seg_idx].points[0];
	  tmp1 = seg_idx;
	  for (j = i; j < n_active_segs; j++)
	    {
	      tmp2 = active_segs[j];
	      active_segs[j] = tmp1;
	      tmp1 = tmp2;
	    }
	  active_segs[n_active_segs] = tmp1;
	  n_active_segs++;
	  seg_idx++;
	}
      first_share = -1;
      share_x = 0;

      for (i = 0; i < n_active_segs; i++)
	{
	  asi = active_segs[i];
	  p_curs = ips[asi][1];
	  if (p_curs.y == y)
	    {
	      bar (new_vp, n_points_max,
		   p_curs, seg_map, active_segs, i);

	      n_ips[asi]--;
	      for (j = 0; j < n_ips[asi]; j++)
		ips[asi][j] = ips[asi][j + 1];

	      if (first_share < 0 || p_curs.x != share_x)
		{
		  foo (first_share, i,
		       active_segs, n_active_segs,
		       cursor, ips, n_ips, n_ips_max, vp, seg_map,
		       &new_vp, &n_segs_max, &n_points_max);
		  first_share = i;
		  share_x = p_curs.x;
		}
	    }
	  else
	    {
	      foo (first_share, i,
		   active_segs, n_active_segs,
		   cursor, ips, n_ips, n_ips_max, vp, seg_map,
		   &new_vp, &n_segs_max, &n_points_max);
	      first_share = -1;
	    }
	}
    }
  return new_vp;
}
