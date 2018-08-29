/* PR c/83222 */

const char a = 0x42;
const double b = (double) a;
const double c = a;
double d = (double) a;
double e = a;
const double f = 1 + (double) a;
const double g = 1 + a;
double h = 1 + (double) a;
double i = 1 + a;
double j[] = { (double) a, a, 1 + (double) a, 1 + a };

void
foo (void)
{
  static const double k = (double) a;
  static const double l = a;
  static const double m = 1 + (double) a;
  static const double n = 1 + a;
}
