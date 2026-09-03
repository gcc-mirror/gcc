/* Check that spills inside a SIMT region go to the AVGPRs.

   In SIMT mode every value is a 64-lane vector, so the 50 values kept live
   below need 50 VGPRs, and a non-kernel function may only use 24 VGPRs (see
   MAX_NORMAL_VGPR_COUNT).  The spills have to preserve all 64 lanes, so on
   devices with AVGPRs they should go there; in particular they must not go
   through v_readlane into an SGPR, which would keep lane 0 only.

   Everything is indexed by the lane ID, so that the values really are
   per-lane and the simt_vect pass converts them to 64-lane vectors: values
   that are the same in every lane stay scalar, and 50 scalars fit in the
   SGPRs without spilling anything.  The loads are volatile so that the
   values have to be spilled rather than simply reloaded.

   This is the SIMT-mode counterpart of avgpr-spill-int.c, which uses vector
   extensions to create the same pressure in SIMD mode.  */

/* { dg-do compile } */
/* { dg-require-effective-target amdgcn_simt } */
/* { dg-additional-options "-march=gfx908 -O1" } */
/* { dg-final { scan-assembler "accvgpr" } } */
/* { dg-final { scan-assembler-not "v_readlane" } } */

#ifndef __SIMT__
#error "This test requires -msimt"
#endif

#define TID __builtin_gcn_dim_pos (2)

#ifndef TYPE
#define TYPE int
#endif
#define N 50

/* Global variables aren't thread-local.  */
volatile TYPE in[64 + N];
TYPE res[N][64];

void __attribute__((noinline,noclone))
f (void)
{
  int A;
  TYPE v0, v1, v2, v3, v4, v5, v6, v7, v8, v9;
  TYPE v10, v11, v12, v13, v14, v15, v16, v17, v18, v19;
  TYPE v20, v21, v22, v23, v24, v25, v26, v27, v28, v29;
  TYPE v30, v31, v32, v33, v34, v35, v36, v37, v38, v39;
  TYPE v40, v41, v42, v43, v44, v45, v46, v47, v48, v49;

  A = __builtin_gcn_simt_enter (1, 2);

  v0 = in[TID + 0];
  v1 = in[TID + 1];
  v2 = in[TID + 2];
  v3 = in[TID + 3];
  v4 = in[TID + 4];
  v5 = in[TID + 5];
  v6 = in[TID + 6];
  v7 = in[TID + 7];
  v8 = in[TID + 8];
  v9 = in[TID + 9];
  v10 = in[TID + 10];
  v11 = in[TID + 11];
  v12 = in[TID + 12];
  v13 = in[TID + 13];
  v14 = in[TID + 14];
  v15 = in[TID + 15];
  v16 = in[TID + 16];
  v17 = in[TID + 17];
  v18 = in[TID + 18];
  v19 = in[TID + 19];
  v20 = in[TID + 20];
  v21 = in[TID + 21];
  v22 = in[TID + 22];
  v23 = in[TID + 23];
  v24 = in[TID + 24];
  v25 = in[TID + 25];
  v26 = in[TID + 26];
  v27 = in[TID + 27];
  v28 = in[TID + 28];
  v29 = in[TID + 29];
  v30 = in[TID + 30];
  v31 = in[TID + 31];
  v32 = in[TID + 32];
  v33 = in[TID + 33];
  v34 = in[TID + 34];
  v35 = in[TID + 35];
  v36 = in[TID + 36];
  v37 = in[TID + 37];
  v38 = in[TID + 38];
  v39 = in[TID + 39];
  v40 = in[TID + 40];
  v41 = in[TID + 41];
  v42 = in[TID + 42];
  v43 = in[TID + 43];
  v44 = in[TID + 44];
  v45 = in[TID + 45];
  v46 = in[TID + 46];
  v47 = in[TID + 47];
  v48 = in[TID + 48];
  v49 = in[TID + 49];

  res[0][TID] = v0;
  res[1][TID] = v1;
  res[2][TID] = v2;
  res[3][TID] = v3;
  res[4][TID] = v4;
  res[5][TID] = v5;
  res[6][TID] = v6;
  res[7][TID] = v7;
  res[8][TID] = v8;
  res[9][TID] = v9;
  res[10][TID] = v10;
  res[11][TID] = v11;
  res[12][TID] = v12;
  res[13][TID] = v13;
  res[14][TID] = v14;
  res[15][TID] = v15;
  res[16][TID] = v16;
  res[17][TID] = v17;
  res[18][TID] = v18;
  res[19][TID] = v19;
  res[20][TID] = v20;
  res[21][TID] = v21;
  res[22][TID] = v22;
  res[23][TID] = v23;
  res[24][TID] = v24;
  res[25][TID] = v25;
  res[26][TID] = v26;
  res[27][TID] = v27;
  res[28][TID] = v28;
  res[29][TID] = v29;
  res[30][TID] = v30;
  res[31][TID] = v31;
  res[32][TID] = v32;
  res[33][TID] = v33;
  res[34][TID] = v34;
  res[35][TID] = v35;
  res[36][TID] = v36;
  res[37][TID] = v37;
  res[38][TID] = v38;
  res[39][TID] = v39;
  res[40][TID] = v40;
  res[41][TID] = v41;
  res[42][TID] = v42;
  res[43][TID] = v43;
  res[44][TID] = v44;
  res[45][TID] = v45;
  res[46][TID] = v46;
  res[47][TID] = v47;
  res[48][TID] = v48;
  res[49][TID] = v49;

  __builtin_gcn_simt_exit (A);
}
