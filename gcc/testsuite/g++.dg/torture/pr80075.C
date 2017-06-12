// { dg-do compile }
// { dg-additional-options "-fnon-call-exceptions" }

struct s {
    int i;
};

extern int use_memcpy;
extern void my_memcpy(void*, void*, int);

int
f (struct s* p)
{
  struct s a;

  try
    {
      a = (struct s){};
      if (!use_memcpy)
	*p = a;
      else
	my_memcpy (p, &a, sizeof (struct s));
    } catch (...) {
	return 0;
    }
  return 1;
}
