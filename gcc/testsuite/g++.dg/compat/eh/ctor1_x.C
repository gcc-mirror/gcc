extern "C" void abort (void);
extern "C" void exit (int);

#include "ctor1.h"

bool was_f_in_Bar_destroyed=false;

void ctor1_x ()
{
  try
    {
      Bar f; 
    }
  catch(int i)
    {
      if(was_f_in_Bar_destroyed)
	{
	  exit (0);
	}
    }
  abort ();
}
