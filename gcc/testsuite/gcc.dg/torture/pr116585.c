/* { dg-do compile } */

char *s1, *s2;
extern int* my_alloc (int);
extern int _setjmp ();
extern void bar();
void foo(int s1len, int s2len)
{
  int e;
  e = _setjmp ();
    {
      int l, i;
      int *md = my_alloc(((sizeof(int)) * (s1len + 1) * (s2len)));
      s1len++;
      for (; s1len; l)
	for (; s2len; l)
	  for (; s1len; i)
	    {
	      int j = 1;
	      for (; j < s2len; j++)
		{
		  int cost;
		  if (s1[1] == s2[1])
		    cost = 0;
		  else
		    cost = 1;
		  md[j * s1len ] = ((cost));
		}
	    }
      bar();
    }
}
