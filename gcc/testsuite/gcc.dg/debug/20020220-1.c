/* PR debug/4461
   This testcase failed to link in Dwarf-2 because
   constant -4.0 in constant pool was never referenced by anything
   but Dwarf-2 location descriptor.  */
/* { dg-do run } */

void __attribute__((noinline))
foo (const char *x __attribute__((unused)),
     __complex__ long double y __attribute__((unused)),
     __complex__ long double z __attribute__((unused)))
{
}

void
bar (void)
{
  foo ("",
       __builtin_conjl (({ __complex__ long double r;
			   __real__ r = 3.0;
			   __imag__ r = -4.0;
			   r; })),
       ({ __complex__ long double s;
	  __real__ s = 3.0;
	  __imag__ s = 4.0;
	  s; }));
}

int main (void)
{
  return 0;
}
