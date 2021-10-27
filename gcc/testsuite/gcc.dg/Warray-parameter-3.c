/* PR c/50584 - No warning for passing small array to C99 static array
   declarator
   { dg-do compile }
   { dg-options "-Wall -Warray-parameter=1" } */

/* Verify that at level 1 mismatches in the bounds of ordinary array
   parameters don't trigger -Warray-parameter.  */
void fax (int[]);
void fax (int[0]);
void fax (int[1]);
void fax (int[2]);
void fax (int[3]);

/* Same as above but starting with an array with a specified bound.  */
void gax (int[3]);
void gax (int[2]);
void gax (int[1]);
void gax (int[0]);
void gax (int[]);

/* Same for multidimensional arrays.  */
void fax_y (int[][3]);
void fax_y (int[0][3]);
void fax_y (int[1][3]);
void fax_y (int[2][3]);
void fax_y (int[3][3]);

/* Same as above but starting with an array with a specified bound.  */
void gax_y (int[3][5]);
void gax_y (int[2][5]);
void gax_y (int[1][5]);
void gax_y (int[0][5]);
void gax_y (int[][5]);

/* Exercise VLAs with a mismatch in the bound for an ordinary array.  */
void fvlax_y (int n, int[][n]);
void fvlax_y (int n, int[0][n]);
void fvlax_y (int n, int[1][n]);
void fvlax_y (int n, int[2][n]);
void fvlax_y (int n, int[3][n]);

void fvlaxn_y (int n, int[][n]);
void fvlaxn_y (int n, int[0][n]);
void fvlaxn_y (int n, int[1][n]);
void fvlaxn_y (int n, int[2][n]);
void fvlaxn_y (int n, int[3][n]);

void fvlaxx_y (int[][*]);
void fvlaxx_y (int[0][*]);
void fvlaxx_y (int[1][*]);
void fvlaxx_y (int[2][*]);
void fvlaxx_y (int[3][*]);

/* Verify that mismatches in the bounds of array parameters declared
   static do trigger -Warray-parameter.  */
void fas1 (int[static 1]);    // { dg-message "previously declared as 'int\\\[static 1]'" }
void fas1 (int[static 2]);    // { dg-warning "\\\[-Warray-parameter=" }


/* Also verify that -Warray-bounds doesn't trigger for ordinary array
   parameters...  */
#pragma GCC optimize ("2")

__attribute__ ((noipa)) void
gca3 (char a[3])
{
  a[0] = 0; a[1] = 1; a[2] = 2; a[3] = 3;
}

__attribute__ ((noipa)) void
gia3 (int a[3])
{
  a[0] = 0; a[1] = 1; a[2] = 2; a[3] = 3;
}

/* ...but does for static arrays.  */
__attribute__ ((noipa)) void
gcas3 (char a[static 3])
{
  a[0] = 0; a[1] = 1; a[2] = 2; // { dg-warning "\\\[-Wstringop-overflow" "pr102706" { target { vect_slp_v4qi_store } } }
  a[3] = 3;                   // { dg-warning "\\\[-Warray-bounds" }
}

__attribute__ ((noipa)) void
gias3 (int a[static 3])
{
  a[0] = 0; a[1] = 1; a[2] = 2;
  a[3] = 3;                   // { dg-warning "\\\[-Warray-bounds" }
}
