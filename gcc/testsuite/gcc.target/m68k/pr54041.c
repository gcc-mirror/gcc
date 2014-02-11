/* { dg-do compile } */
/* { dg-options "-O -mshort" } */

extern int r[];

int *fn(int i)
{
	return &r[i];
}

