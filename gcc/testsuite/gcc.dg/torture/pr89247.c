/* { dg-do compile } */

int *a;
void b()
{
  void *c = &&d;
  for (;;)
    d:
	if (*a)
	  ;
	else
	  *a = ({ 0 < b; });
}
