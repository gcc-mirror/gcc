/* PR java/10145
   Test that requesting an alignment of 1 does not increase the alignment
   of a long long field.

   { dg-do run }
   { dg-options "" }
*/

extern void abort (void);

struct A
{
  char c;
  long long i;
};

struct B
{
  char c;
  long long i __attribute ((__aligned__ (1)));
};

int main ()
{
  if (sizeof (struct A) != sizeof (struct B))
    abort ();
  return 0;
}
