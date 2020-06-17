/* { dg-do compile } */

short c;
_Bool d;
unsigned e, f;
char g, h;
extern _Bool i[];
void j()
{
  for (char a = 0; a < 100; a++)
    for (char b = 0; b < 20; b += 2)
      {
	if (e)
	  d = f = 0;
	else
	  g = i[8] = 0;
	h = c;
      }
}
