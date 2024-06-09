void exit (int);

typedef struct
{
  short f:3, g:3, h:10;
} small;

struct
{
  int i;
  small s[10];
} x;

int
main (void)
{
  int i;
  for (i = 0; i < 10; i++)
    x.s[i].f = 0;
  exit (0);
}
