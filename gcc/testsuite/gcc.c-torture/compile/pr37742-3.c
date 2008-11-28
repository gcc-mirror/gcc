void matmul_i4 (int * __restrict dest_y,
		const int * __restrict abase,
		const int * __restrict bbase_y,
		int count, int xcount, int ycount, int aystride)
{               
  int x, y, n;
  const int * __restrict abase_n;
  int bbase_yn;
  for (y = 0; y < ycount; y++)
    for (n = 0; n < count; n++) {
	abase_n = abase + n*aystride;
	bbase_yn = bbase_y[n];
	for (x = 0; x < xcount; x++)
	  dest_y[x] += abase_n[x] * bbase_yn; 
    }
}

