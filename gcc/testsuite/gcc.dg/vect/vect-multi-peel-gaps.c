/* For some targets we end up vectorizing the below loop such that the `sp`
   single integer is loaded into a 4 integer vector.
   While the writes are all safe, without 2 scalar loops being peeled into the
   epilogue we would read past the end of the 31 integer array.  This happens
   because we load a 4 integer chunk to only use the first integer and
   increment by 2 integers at a time, hence the last load needs s[30-33] and
   the penultimate load needs s[28-31].
   This testcase ensures that we do not crash due to that behaviour.  */
/* { dg-require-effective-target mmap } */
#include <sys/mman.h>
#include <stdio.h>
#include "tree-vect.h"

#define MMAP_SIZE 0x20000
#define ADDRESS 0x1122000000

#define MB_BLOCK_SIZE 16
#define VERT_PRED_16 0
#define HOR_PRED_16 1
#define DC_PRED_16 2
int *sptr;
extern void intrapred_luma_16x16();
unsigned short mprr_2[5][16][16];
void initialise_s(int *s) { }
int main() {
    void *s_mapping;
    void *end_s;
    check_vect ();
    s_mapping = mmap ((void *)ADDRESS, MMAP_SIZE, PROT_READ | PROT_WRITE,
		      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (s_mapping == MAP_FAILED)
      {
	perror ("mmap");
	return 1;
      }
    end_s = (s_mapping + MMAP_SIZE);
    sptr = (int*)(end_s - sizeof(int[31]));
    intrapred_luma_16x16(sptr);
    return 0;
}

void intrapred_luma_16x16(int * restrict sp) {
    for (int j=0; j < MB_BLOCK_SIZE; j++)
      {
	mprr_2[VERT_PRED_16][j][0]=sp[j*2];
	mprr_2[VERT_PRED_16][j][1]=sp[j*2];
	mprr_2[VERT_PRED_16][j][2]=sp[j*2];
	mprr_2[VERT_PRED_16][j][3]=sp[j*2];
	mprr_2[VERT_PRED_16][j][4]=sp[j*2];
	mprr_2[VERT_PRED_16][j][5]=sp[j*2];
	mprr_2[VERT_PRED_16][j][6]=sp[j*2];
	mprr_2[VERT_PRED_16][j][7]=sp[j*2];
	mprr_2[VERT_PRED_16][j][8]=sp[j*2];
	mprr_2[VERT_PRED_16][j][9]=sp[j*2];
	mprr_2[VERT_PRED_16][j][10]=sp[j*2];
	mprr_2[VERT_PRED_16][j][11]=sp[j*2];
	mprr_2[VERT_PRED_16][j][12]=sp[j*2];
	mprr_2[VERT_PRED_16][j][13]=sp[j*2];
	mprr_2[VERT_PRED_16][j][14]=sp[j*2];
	mprr_2[VERT_PRED_16][j][15]=sp[j*2];
      }
}
/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" {target { vect_int && vect_perm } } } } */
