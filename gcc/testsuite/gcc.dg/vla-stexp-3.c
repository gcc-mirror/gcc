/* PR91038 */
/* { dg-do compile } */
/* { dg-options "" } */


void bar(void)
{
	({ int N = 2; int (*x)[9][N] = 0; x; })[1];
	({ int N = 2; int (*x)[9][N] = 0; x; })[0];	// should not ice
}

