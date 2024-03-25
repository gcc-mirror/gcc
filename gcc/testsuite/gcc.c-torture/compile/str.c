/* { dg-additional-options "-std=gnu89" } */

typedef struct
{
  char a;
  char b;
} foo;

bar ()
{
  foo foobar[100];
  foobar[1].a = 'a';
  foobar[2].a = 'b';
  barfoo (foobar);
}
