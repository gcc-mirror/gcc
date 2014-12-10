/* { dg-do compile } */

extern int abs (int);
extern void attenuate_curve(int*);
int a;
void
setup_tone_curves_center_boost ()
{
  int b[0];
  a = 0;
  for (; a < 6; a++)
    {
      int c = abs (a);
      b[a] = c;
    }
  attenuate_curve (b);
}
