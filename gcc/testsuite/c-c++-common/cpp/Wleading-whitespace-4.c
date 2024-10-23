/* { dg-do compile { target { c || c++11 } } } */
/* { dg-options "-Wleading-whitespace=none" } */

	int i1; /* tab ok */
			int i2; /* 3 tabs ok */
        int i3; /* 8 spaces ok */
	        int i4; /* tab 8 spaces ok */
    	int i5; /* 4 spaces tab ok */
 	int i6; /* space tab ok */
	 int i7; /* tab vtab ok */
	 int i8; /* tab form-feed ok */
     int i9; /* 4 spaces vtab ok */
   int i10; /* 2 spaces form-feed ok */
	        	
/* Just whitespace on a line is something for -Wtrailing-whitespace.  */
int \
	i11, \
    i12, \
		        i13, \
        i14, \
 	i15, \
		i16, \
		i17;
          const char *p = R"*|*(		
    a
	b
	    c
          d
 	e
		 f
  g
)*|*";
/* This is a comment with leading whitespace non-issues and issues
		a
      b
	    c
	        d
 	e
	 f
	 g
*/
