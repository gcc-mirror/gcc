/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors -Wvla-parameter" } */

void fix_fix (int i,
	      char (*a)[3][5],
	      int (*x)[_Countof (*a)],
	      short (*)[_Generic(x, int (*)[3]: 1)]);
void fix_var (int i,
	      char (*a)[3][i], /* dg-warn "variable" */
	      int (*x)[_Countof (*a)],
	      short (*)[_Generic(x, int (*)[3]: 1)]);
void fix_uns (int i,
	      char (*a)[3][*],
	      int (*x)[_Countof (*a)],
	      short (*)[_Generic(x, int (*)[3]: 1)]);

void var_fix (int i,
	      char (*a)[i][5], /* dg-warn "variable" */
	      int (*x)[_Countof (*a)]); /* dg-warn "variable" */
void var_var (int i,
	      char (*a)[i][i], /* dg-warn "variable" */
	      int (*x)[_Countof (*a)]); /* dg-warn "variable" */
void var_uns (int i,
	      char (*a)[i][*], /* dg-warn "variable" */
	      int (*x)[_Countof (*a)]); /* dg-warn "variable" */

void uns_fix (int i,
	      char (*a)[*][5],
	      int (*x)[_Countof (*a)]);
void uns_var (int i,
	      char (*a)[*][i], /* dg-warn "variable" */
	      int (*x)[_Countof (*a)]);
void uns_uns (int i,
	      char (*a)[*][*],
	      int (*x)[_Countof (*a)]);
