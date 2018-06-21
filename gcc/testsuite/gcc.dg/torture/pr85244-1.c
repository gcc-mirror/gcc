/* { dg-do run } */
/* { dg-additional-sources "pr85244-2.c" } */

struct s {
 long a;
 int b;
 int tab[];
};

extern const struct s val;
extern int idx;
extern void abort (void);

int main()
{
  if (val.tab[0] != 42 || val.tab[1] != 1337 || val.tab[idx] != 1337)
    abort ();
  return 0;
}
