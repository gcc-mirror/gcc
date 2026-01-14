/* Testing the attribute counted_by for anonymous structures as plan9-extensions.
   used in bounds sanitizer.  */
/* { dg-do run } */
/* { dg-options "-O2 -fplan9-extensions -fsanitize=bounds" } */
/* { dg-output "index 12 out of bounds for type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 22 out of bounds for type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 31 out of bounds for type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

struct ids 
{
  int length_ad;
  int length_na;
};

typedef union 
{
  int length_hb;
  float other;
} ids_2;

struct person
{
  int age;
  int weight;
  struct ids;    // Anonymous structure, no name needed
  ids_2; // Anonymous union, no name needed
  char *address __attribute__ ((counted_by (length_ad))); 
  char *hobby __attribute__ ((counted_by (length_hb)));
  char name[]  __attribute__ ((counted_by (length_na)));
} *Jim;

static void
set_counted_by (struct ids *p, ids_2 *p2,
                int address_l, int name_l, int hb_l)
{
  p->length_ad = address_l;
  p->length_na = name_l;
  p2->length_hb = hb_l;
}

static void
setup (int address_l, int name_l, int hb_l)
{
  Jim = (struct person *) __builtin_malloc (sizeof (struct person)
					    + name_l * sizeof (char));
  Jim->address = (char *) __builtin_malloc (sizeof (char) * address_l);
  Jim->hobby = (char *) __builtin_malloc (sizeof (char) * hb_l);
  set_counted_by (Jim, Jim, address_l, name_l, hb_l);
}

static void
cleanup ()
{
  __builtin_free (Jim->address);
  __builtin_free (Jim->hobby);
  __builtin_free (Jim);
}

int main()
{
  setup (20, 10, 30);
  Jim->name[12] = 'a';
  Jim->address[22] = 'k';
  Jim->hobby[31] = 'h';
  return 0;
}
