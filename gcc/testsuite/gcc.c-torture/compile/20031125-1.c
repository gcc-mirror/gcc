/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */

short *_offsetTable;
/* This tests to make sure PRE splits the entry block ->block 0 edge
   when there are multiple block 0 predecessors.
   This is done so that we don't end up with an insertion on the 
   entry block -> block 0 edge which would require a split at insertion
   time.  
   PR 13163.  */
void proc4WithoutFDFE(char *dst, const char *src, int next_offs, int bw,
		int bh, int pitch)
{
	do {
		int i = bw;
		int code = *src++;
		int x, l;
		int length = *src++ + 1;

		for (l = 0; l < length; l++) {
			int x;

			for (x = 0; x < 4; x++) ;
			if (i == 0)
				dst += pitch * 3;
		}
		char *dst2 = dst + _offsetTable[code] + next_offs;

		for (x = 0; x < 4; x++) {
			int j = 0;
			(dst + pitch * x)[j] = (dst2 + pitch * x)[j];
		}
		dst += pitch * 3;
	} while (--bh);
}


