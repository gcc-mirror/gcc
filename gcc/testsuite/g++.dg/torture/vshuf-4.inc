/* Test fragment for vectors of 4 elements.  */

#ifndef UNSUPPORTED

constexpr V in1[] = { {A, B, C, D}, {A, B, C, D}, {A, B, C, D}, {A, B, C, D},
		     {A, B, C, D}, {W, X, Y, Z}, {W, X, Y, Z}, {W, X, Y, Z}};

constexpr VI mask1[] = { {0, 1, 2, 3}, {0+1*4, 1+2*4, 2+3*4, 3+4*4}, {3, 2, 1, 0},
			 {0, 3, 2, 1}, {0, 2, 1, 3}, {3, 1, 2, 0}, {0, 0, 0, 0},
			 {1, 2, 1, 2}};

constexpr V out1[] = { { A, B, C, D}, {A, B, C, D}, {D, C, B, A}, {A, D, C, B},
		       { A, C, B, D}, {Z, X, Y, W}, {W, W, W, W}, {X, Y, X, Y}};


constexpr V in2[] = { {A, B, C, D}, {A, B, C, D}, {A, B, C, D}, {A, B, C, D},
		      {A, B, C, D}, {A, B, C, D}};
constexpr V in3 = {W, X, Y, Z};
constexpr VI mask2[] = { {0, 1, 2, 3}, {4, 5, 6, 7}, {0, 4, 1, 5}, {0, 7, 4, 3},
			 {0, 0, 0, 0}, {7, 7, 7, 7}};
constexpr V out2[] = { {A, B, C, D}, {W, X, Y, Z}, {A, W, B, X}, {A, Z, W, D},
		       {A, A, A, A}, {Z, Z, Z, Z}};


#endif
