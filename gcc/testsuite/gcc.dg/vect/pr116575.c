/* { dg-do compile } */

int a;
float *b, *c;
void d(char * __restrict e)
{
  for (; a; a++, b += 4, c += 4)
    if (*e++) {
	float *f = c;
	f[0] = b[0];
	f[1] = b[1];
	f[2] = b[2];
	f[3] = b[3];
    }
}
