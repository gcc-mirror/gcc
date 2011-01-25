/* PR tree-optimization/18792 */
/* { dg-do compile } */
/* { dg-options "-O1 -ftree-loop-linear" } */
void put_atoms_in_triclinic_unitcell(float x[][3])
{
	int i=0,d;

	while (x[i][3] < 0)
		for (d=0; d<=3; d++)
			x[i][d] = 0;

	while (x[i][3] >= 0)
		for (d=0; d<=3; d++)
			x[i][d] = 0;

}
