/* { dg-options "-O2 -fgraphite-identity" } */
typedef long int integer;
typedef double doublereal;

static int balanc_(nm, n, a, low, igh, scale)
doublereal *a;
{
 integer a_dim1, a_offset, i__1, i__2;
 integer iexc;
 integer i__, j, k, l, m;
 integer jj;
goto L100;
L20:
if (j == m) {
goto L50;
}
for (i__ = 1; i__ <= i__1; ++i__) {
a[i__ + j * a_dim1] = a[i__ + m * a_dim1];
}
L50:
switch ((int)iexc) {
case 2: goto L130;
}
L100:
for (jj = 1; jj <= i__1; ++jj) {
goto L20;
}
L130:
for (j = k; j <= i__1; ++j) {
goto L20;
}
}

int pymol_rg_(integer *nm, integer *n, doublereal *a, doublereal *wr,
              doublereal *fv1,integer *ierr)
{
    integer a_dim1, a_offset, z_dim1, z_offset;
    integer is1, is2;
    balanc_(nm, n, &a[a_offset], &is1, &is2, &fv1[1]);
}
