/* From PR target/16176 */
struct __attribute__ ((packed)) s { struct s *next; };

struct s * __attribute__ ((noinline))
maybe_next (struct s *s, int t)
{
  if (t)
    s = s->next;
  return s;
}

int main ()
{
  struct s s1, s2;

  s1.next = &s2;
  if (maybe_next (&s1, 1) != &s2)
    abort ();
  exit (0);
}
