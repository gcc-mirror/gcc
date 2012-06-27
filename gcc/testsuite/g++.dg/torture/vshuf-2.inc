/* Test fragment for vectors of 2 elements.  */

#ifndef UNSUPPORTED

constexpr V in1[] = { { A, B}, {A, B}, { A, B},
	    	      { A, B}, {X, Y}, { X, Y}};
constexpr VI mask1[] = { {0, 1}, {(unsigned)-16, 1}, {1, 0},
	     	         {0, 0}, {  1, 1}, {1, 0}};
constexpr V out1[] = { {A, B}, {A, B}, {B, A},
	    	       {A, A}, {Y, Y}, {Y, X}};
	    	  
constexpr V in2[] = { { A, B}, {A, B}, {A, B}, {A, B},
	    	      { A, B}, {A, B}, {A, B}};
constexpr V in3 = {X, Y};
constexpr VI mask2[] = { {0, 1}, {2, 3}, {0, 2}, {2, 1},
	     	         {3, 0}, {0, 0}, {3, 3}};

constexpr V out2[] = { {A, B}, {X, Y}, {A, X}, {X, B},
	    	       {Y, A}, {A, A}, {Y, Y}};


#endif
