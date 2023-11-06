/* When compiled with -pedantic, this program will cause an ICE when the
   constant propagator tries to set the value of *str to UNDEFINED.
   
   This happens because *str is erroneously considered as a store alias.
   The aliasing code is then making *str an alias leader for its alias set
   and when the PHI node at the end of the while() is visited the first
   time, CCP will try to assign it a value of UNDEFINED, but the default
   value for *str is a constant.  */
typedef	__SIZE_TYPE__ size_t;
size_t strlength (const char * const);
char foo();

static const char * const str = "mingo";

int
bar(void)
{
  size_t c;
  char *x;

  c = strlength (str);
  while (c < 10)
    {
      if (c > 5)
	*x = foo ();
      if (*x < 'a')
	break;
    }

  return *x == '3';
}
