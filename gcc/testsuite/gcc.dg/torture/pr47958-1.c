/* { dg-do assemble } */
/* { dg-additional-options "-Wno-old-style-definition" } */

void (*foo[6][6]) (int);
void bar (hdR)
    int hdR;
{ }
void xxx ()
{
    unsigned int i, j;
    for (i = 0; i < 6; ++i)
	for (j = 0; j < 6; ++j)
            foo [i][j] = bar;
}
