/* { dg-do compile } */
/* { dg-require-ifunc "" } */

__attribute__((target_clones("default", "default")))
int foo (); /* { dg-error "multiple 'default' targets were set" } */

int
bar ()
{
  return foo();
}
