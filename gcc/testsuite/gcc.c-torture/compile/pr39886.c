/* PR middle-end/39886 */

int foo (int);
void func (int);

volatile unsigned char g;

void bar (int p)
{
  char l = 0xE1;
  func ((foo ((p & g) <= l), 1));
}

