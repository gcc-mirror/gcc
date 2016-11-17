/* { dg-options "-mlock" } */
/* { dg-do assemble } */
/* { dg-skip-if "" { arc6xx } } */

int f (void *p)
{
  int i;

  __asm__("llock %0, [%1]\n\t"
	  "scond %0, [%1]" : "=&r"(i) : "r"(p));
  return i;
}
