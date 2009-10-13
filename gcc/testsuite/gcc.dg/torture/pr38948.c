/* { dg-options "-fno-tree-sra" } */
/* { dg-options "-fno-tree-sra -march=v32" { target cris-*-* } } */
typedef unsigned char byte;
typedef unsigned int uint;
typedef int bool;
typedef struct gs_const_string_s
{
  const byte *data;
}
gs_const_string;
struct gs_matrix_s
{
  float xx, xy, yx, yy, tx, ty;
};
typedef struct gs_matrix_s gs_matrix;
typedef long fixed;
typedef struct gs_fixed_point_s
{
  fixed x, y;
}
gs_fixed_point;
typedef struct gs_matrix_fixed_s
{
  int x;
}
gs_matrix_fixed;
static int
append_simple (const byte * glyph, const gs_matrix_fixed * pmat, void * ppath)
{
  int numContours =
    (int) (((((uint) ((glyph)[0]) << 8) + (glyph)[1]) ^ 0x8000) - 0x8000);
  const byte *pends = glyph + 10;
  int code = 0;
  {
    uint i = 0;
    uint np = 0;
    gs_fixed_point pt = {0};
    uint reps = 0;
    for (i = 0, np = 0; i < numContours; ++i)
      {
	bool move = ((bool) 1);
	uint last_point =
	  (((uint) ((pends + i * 2)[0]) << 8) + (pends + i * 2)[1]);
	int off_curve = 0;
	gs_fixed_point cpoints[3];
	for (; np <= last_point; --reps, ++np)
	  {
	    if (move)
	      {
		cpoints[0] = pt;
		move = ((bool) 0);
	      }
	    else
	      {
		switch (off_curve++)
		  {
		  default:
		    cpoints[2].x = ((cpoints[1].x + pt.x) / 2);
		    cpoints[2].y = ((cpoints[1].y + pt.y) / 2);
		    code =
		      gx_path_add_curve_notes (ppath,
					       ((cpoints[0].x +
						 2 * cpoints[1].x) / 3),
					       ((cpoints[0].y +
						 2 * cpoints[1].y) / 3),
					       ((2 * cpoints[1].x +
						 cpoints[2].x) / 3),
					       ((2 * cpoints[1].y +
						 cpoints[2].y) / 3),
					       cpoints[2].x, cpoints[2].y,
					       0);
		    cpoints[0] = cpoints[2];
		  case 0:
		    cpoints[1] = pt;
		  }
	      }
	  }
      }
  }
}
int
append_outline (uint glyph_index, const gs_matrix_fixed *pmat, void *ppath)
{
  gs_const_string glyph_string = {0};
  int numContours = 0;
  numContours =
    (int) (((((uint) ((glyph_string.data)[0]) << 8) +
	     (glyph_string.data)[1]) ^ 0x8000) - 0x8000);
  if (numContours >= 0)
    return append_simple (glyph_string.data, pmat, ppath);
  {
    uint flags = 0;
    do
      {
	gs_matrix_fixed mat = {0};
	gs_matrix scale_mat = {0};
	gs_matrix_multiply (&scale_mat, (const gs_matrix *) &mat, (gs_matrix *) & mat);
      }
    while (flags & 32);
  }
}
