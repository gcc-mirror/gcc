/* PR rtl-opt/15289 */

typedef struct { _Complex char a; _Complex char b; } Scc2;

Scc2 s = { 1+2i, 3+4i };

int checkScc2 (Scc2 s)
{
  return s.a != 1+2i || s.b != 3+4i;
}

int main (void)
{
  return checkScc2 (s);
}
