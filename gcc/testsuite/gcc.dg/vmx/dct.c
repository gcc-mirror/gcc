/* { dg-do compile } */
#include <altivec.h>

inline void
transpose_vmx (vector signed short *input, vector signed short *output)
{
  vector signed short v0, v1, v2, v3, v4, v5, v6, v7;
  vector signed short x0, x1, x2, x3, x4, x5, x6, x7;

  /* Matrix transpose */
  v0 = vec_mergeh (input[0], input[4]);
  v1 = vec_mergel (input[0], input[4]);
  v2 = vec_mergeh (input[1], input[5]);
  v3 = vec_mergel (input[1], input[5]);
  v4 = vec_mergeh (input[2], input[6]);
  v5 = vec_mergel (input[2], input[6]);
  v6 = vec_mergeh (input[3], input[7]);
  v7 = vec_mergel (input[3], input[7]);

  x0 = vec_mergeh (v0, v4);
  x1 = vec_mergel (v0, v4);
  x2 = vec_mergeh (v1, v5);
  x3 = vec_mergel (v1, v5);
  x4 = vec_mergeh (v2, v6);
  x5 = vec_mergel (v2, v6);
  x6 = vec_mergeh (v3, v7);
  x7 = vec_mergel (v3, v7);

  output[0] = vec_mergeh (x0, x4);
  output[1] = vec_mergel (x0, x4);
  output[2] = vec_mergeh (x1, x5);
  output[3] = vec_mergel (x1, x5);
  output[4] = vec_mergeh (x2, x6);
  output[5] = vec_mergel (x2, x6);
  output[6] = vec_mergeh (x3, x7);
  output[7] = vec_mergel (x3, x7);
}

void
dct_vmx (vector signed short *input, vector signed short *output,
	 vector signed short *postscale)
{
  vector signed short mul0, mul1, mul2, mul3, mul4, mul5, mul6, mul;
  vector signed short v0, v1, v2, v3, v4, v5, v6, v7, v8, v9;
  vector signed short v20, v21, v22, v23, v24, v25, v26, v27, v31;
  int i;
  vector signed short in[8], out[8];

  /* Load first eight rows of input data */

  /* Load multiplication constants */

  /* Splat multiplication constants */
  mul0 = vec_splat(input[8],0);
  mul1 = vec_splat(input[8],1);
  mul2 = vec_splat(input[8],2);
  mul3 = vec_splat(input[8],3);
  mul4 = vec_splat(input[8],4);
  mul5 = vec_splat(input[8],5);
  mul6 = vec_splat(input[8],6);

  /* Perform DCT on the eight columns */

  /*********** Stage 1 ***********/

  v8 = vec_adds (input[0], input[7]);
  v9 = vec_subs (input[0], input[7]);
  v0 = vec_adds (input[1], input[6]);
  v7 = vec_subs (input[1], input[6]);
  v1 = vec_adds (input[2], input[5]);
  v6 = vec_subs (input[2], input[5]);
  v2 = vec_adds (input[3], input[4]);
  v5 = vec_subs (input[3], input[4]);

  /*********** Stage 2 ***********/

  /* Top */
  v3 = vec_adds (v8, v2);		/* (V0+V7) + (V3+V4) */
  v4 = vec_subs (v8, v2);		/* (V0+V7) - (V3+V4) */
  v2 = vec_adds (v0, v1);		/* (V1+V6) + (V2+V5) */
  v8 = vec_subs (v0, v1);		/* (V1+V6) - (V2+V5) */

  /* Bottom */
  v0 = vec_subs (v7, v6);		/* (V1-V6) - (V2-V5) */
  v1 = vec_adds (v7, v6);		/* (V1-V6) + (V2-V5) */

  /*********** Stage 3 ***********/

  /* Top */
  in[0] = vec_adds (v3, v2);		/* y0 = v3 + v2 */
  in[4] = vec_subs (v3, v2);		/* y4 = v3 - v2 */
  in[2] = vec_mradds (v8, mul2, v4);	/* y2 = v8 * a0 + v4 */
  v6 = vec_mradds (v4, mul2, mul6);	
  in[6] = vec_subs (v6, v8);		/* y6 = v4 * a0 - v8 */

  /* Bottom */
  v6 = vec_mradds (v0, mul0, v5);	/* v6 = v0 * (c4) + v5 */
  v7 = vec_mradds (v0, mul4, v5);	/* v7 = v0 * (-c4) + v5 */
  v2 = vec_mradds (v1, mul4, v9);	/* v2 = v1 * (-c4) + v9 */
  v3 = vec_mradds (v1, mul0, v9);	/* v3 = v1 * (c4) + v9 */

  /*********** Stage 4 ***********/

  /* Bottom */
  in[1] = vec_mradds (v6, mul3, v3);	/* y1 = v6 * (a1) + v3 */
  v23 = vec_mradds (v3, mul3, mul6);
  in[7] = vec_subs (v23, v6);		/* y7 = v3 * (a1) - v6 */
  in[5] = vec_mradds (v2, mul1, v7);	/* y5 = v2 * (a2) + v7 */
  in[3] = vec_mradds (v7, mul5, v2);	/* y3 = v7 * (-a2) + v2 */

  transpose_vmx (in, out);

  /* Perform DCT on the eight rows */

  /*********** Stage 1 ***********/

  v8 = vec_adds (out[0], out[7]);
  v9 = vec_subs (out[0], out[7]);
  v0 = vec_adds (out[1], out[6]);
  v7 = vec_subs (out[1], out[6]);
  v1 = vec_adds (out[2], out[5]);
  v6 = vec_subs (out[2], out[5]);
  v2 = vec_adds (out[3], out[4]);
  v5 = vec_subs (out[3], out[4]);

  /*********** Stage 2 ***********/

  /* Top */
  v3 = vec_adds (v8, v2);		/* (V0+V7) + (V3+V4) */
  v4 = vec_subs (v8, v2);		/* (V0+V7) - (V3+V4) */
  v2 = vec_adds (v0, v1);		/* (V1+V6) + (V2+V5) */
  v8 = vec_subs (v0, v1);		/* (V1+V6) - (V2+V5) */

  /* Bottom */
  v0 = vec_subs (v7, v6);		/* (V1-V6) - (V2-V5) */
  v1 = vec_adds (v7, v6);		/* (V1-V6) + (V2-V5) */

  /*********** Stage 3 ***********/

  /* Top */
  v25 = vec_subs (v25, v25);          /* reinit v25 = 0 */

  v20 = vec_adds (v3, v2);		/* y0 = v3 + v2 */
  v24 = vec_subs (v3, v2);		/* y4 = v3 - v2 */
  v22 = vec_mradds (v8, mul2, v4);	/* y2 = v8 * a0 + v4 */
  v6 = vec_mradds (v4, mul2, v25);	
  v26 = vec_subs (v6, v8);		/* y6 = v4 * a0 - v8 */

  /* Bottom */
  v6 = vec_mradds (v0, mul0, v5);	/* v6 = v0 * (c4) + v5 */
  v7 = vec_mradds (v0, mul4, v5);	/* v7 = v0 * (-c4) + v5 */
  v2 = vec_mradds (v1, mul4, v9);	/* v2 = v1 * (-c4) + v9 */
  v3 = vec_mradds (v1, mul0, v9);	/* v3 = v1 * (c4) + v9 */

  /*********** Stage 4 ***********/

  /* Bottom */
  v21 = vec_mradds (v6, mul3, v3);	/* y1 = v6 * (a1) + v3 */
  v23 = vec_mradds (v3, mul3, v25);
  v27 = vec_subs (v23, v6);		/* y7 = v3 * (a1) - v6 */
  v25 = vec_mradds (v2, mul1, v7);	/* y5 = v2 * (a2) + v7 */
  v23 = vec_mradds (v7, mul5, v2);	/* y3 = v7 * (-a2) + v2 */

  /* Post-scale and store reults */

  v31 = vec_subs (v31, v31);          /* reinit v25 = 0 */

  output[0] = vec_mradds (postscale[0], v20, v31);
  output[2] = vec_mradds (postscale[2], v22, v31);
  output[4] = vec_mradds (postscale[4], v24, v31);
  output[6] = vec_mradds (postscale[6], v26, v31);
  output[1] = vec_mradds (postscale[1], v21, v31);
  output[3] = vec_mradds (postscale[3], v23, v31);
  output[5] = vec_mradds (postscale[5], v25, v31);
  output[7] = vec_mradds (postscale[7], v27, v31);
}
