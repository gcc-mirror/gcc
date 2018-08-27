// { dg-do compile }

typedef long unsigned int size_t;
extern void fancy_abort () __attribute__ ((__noreturn__));
class cpp_string_location_reader { };
class cpp_substring_ranges {
public:
    void add_range ();
};
typedef unsigned char uchar;
void
cpp_interpret_string_1 (size_t count, cpp_string_location_reader *loc_readers,    cpp_substring_ranges *ranges, uchar c, const uchar *p)
{
  size_t i;
  ((void)(!((loc_readers !=   __null  ) == (ranges !=   __null  )) ? fancy_abort (), 0 : 0));
  cpp_string_location_reader *loc_reader = __null;
  for (i = 0; i < count; i++) 
    {
      if (loc_readers)  loc_reader = &loc_readers[i];
      if (*p == 'R') 	continue;
      for (;;) 	
	{
	  switch (c)     {
	    case 'x':       if (ranges) ranges->add_range (); break;
	    case '7':       ((void)(!((loc_reader !=   __null  ) == (ranges !=   __null  )) ? fancy_abort (), 0 : 0)); break;
	  }
	  p = 0;
	}
    }
}
