typedef struct
{
  short f:3, g:3, h:10;
} small;

struct
{
  int i;
  small s[10];
} x;

main ()
{
  int i;
  for (i = 0; i < 10; i++)
    x.s[i].f = 0;
  exit (0);
}
