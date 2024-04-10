/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

#define RISCV_MATH_LOOPUNROLL
#define RISCV_MATH_VECTOR
typedef  float float32_t;

  typedef struct
  {
          uint16_t numTaps;    /**< number of coefficients in the filter. */
          float32_t *pState;   /**< points to the state variable array. The array is of length numTaps+blockSize-1. */
          float32_t *pCoeffs;  /**< points to the coefficient array. The array is of length numTaps. */
          float32_t mu;        /**< step size that controls filter coefficient updates. */
  } riscv_lms_instance_f32;


void riscv_lms_f32(
  const riscv_lms_instance_f32 * S,
  const float32_t * pSrc,
        float32_t * pRef,
        float32_t * pOut,
        float32_t * pErr,
        uint32_t blockSize)
{
        float32_t *pState = S->pState;                 /* State pointer */
        float32_t *pCoeffs = S->pCoeffs;               /* Coefficient pointer */
        float32_t *pStateCurnt;                        /* Points to the current sample of the state */
        float32_t *px, *pb;                            /* Temporary pointers for state and coefficient buffers */
        float32_t mu = S->mu;                          /* Adaptive factor */
        float32_t acc, e;                              /* Accumulator, error */
        float32_t w;                                   /* Weight factor */
        uint32_t numTaps = S->numTaps;                 /* Number of filter coefficients in the filter */
        uint32_t tapCnt, blkCnt;                       /* Loop counters */

  /* Initializations of error,  difference, Coefficient update */
  e = 0.0f;
  w = 0.0f;

  /* S->pState points to state array which contains previous frame (numTaps - 1) samples */
  /* pStateCurnt points to the location where the new input data should be written */
  pStateCurnt = &(S->pState[(numTaps - 1U)]);

  /* initialise loop count */
  blkCnt = blockSize;

  while (blkCnt > 0U)
  {
    /* Copy the new input sample into the state buffer */
    *pStateCurnt++ = *pSrc++;

    /* Initialize pState pointer */
    px = pState;

    /* Initialize coefficient pointer */
    pb = pCoeffs;

    /* Set the accumulator to zero */
    acc = 0.0f;
    uint32_t vblkCnt = numTaps;                               /* Loop counter */
    size_t l;
    vfloat32m8_t vx, vy;
    vfloat32m1_t temp00m1;
    l = __riscv_vsetvl_e32m1(1);
    temp00m1 = __riscv_vfmv_v_f_f32m1(0, l);
    for (; (l = __riscv_vsetvl_e32m8(vblkCnt)) > 0; vblkCnt -= l) {
      vx = __riscv_vle32_v_f32m8(px, l);
      px += l;
      vy = __riscv_vle32_v_f32m8(pb, l);
      pb += l;
      temp00m1 = __riscv_vfredusum_vs_f32m8_f32m1(__riscv_vfmul_vv_f32m8(vx, vy, l), temp00m1, l);
    }
    acc += __riscv_vfmv_f_s_f32m1_f32(temp00m1);

    while (tapCnt > 0U)
    {
      /* Perform the multiply-accumulate */
      acc += (*px++) * (*pb++);

      /* Decrement the loop counter */
      tapCnt--;
    }
    /* Store the result from accumulator into the destination buffer. */
    *pOut++ = acc;

    /* Compute and store error */
    e = (float32_t) *pRef++ - acc;
    *pErr++ = e;

    /* Calculation of Weighting factor for updating filter coefficients */
    w = e * mu;

    /* Initialize pState pointer */
    /* Advance state pointer by 1 for the next sample */
    px = pState++;

    /* Initialize coefficient pointer */
    pb = pCoeffs;

    vblkCnt = numTaps;
    for (; (l = __riscv_vsetvl_e32m8(vblkCnt)) > 0; vblkCnt -= l) {
      vx = __riscv_vle32_v_f32m8(px, l);
      px += l;
      __riscv_vse32_v_f32m8(pb, __riscv_vfadd_vv_f32m8(__riscv_vfmul_vf_f32m8(vx, w, l), __riscv_vle32_v_f32m8(pb, l), l) , l);
      pb += l;
    }
    while (tapCnt > 0U)
    {
      /* Perform the multiply-accumulate */
      *pb += w * (*px++);
      pb++;

      /* Decrement loop counter */
      tapCnt--;
    }
    /* Decrement loop counter */
    blkCnt--;
  }

  /* Processing is complete.
     Now copy the last numTaps - 1 samples to the start of the state buffer.
     This prepares the state buffer for the next function call. */

  /* Points to the start of the pState buffer */
  pStateCurnt = S->pState;

  /* copy data */

    uint32_t vblkCnt = (numTaps - 1U);                               /* Loop counter */
    size_t l;
    for (; (l = __riscv_vsetvl_e32m8(vblkCnt)) > 0; vblkCnt -= l) {
      __riscv_vse32_v_f32m8(pStateCurnt, __riscv_vle32_v_f32m8(pState, l) , l);
      pState += l;
      pStateCurnt += l;
    }


  /* Loop unrolling: Compute 4 taps at a time. */
  tapCnt = (numTaps - 1U) >> 2U;

  while (tapCnt > 0U)
  {
    *pStateCurnt++ = *pState++;
    *pStateCurnt++ = *pState++;
    *pStateCurnt++ = *pState++;
    *pStateCurnt++ = *pState++;

    /* Decrement loop counter */
    tapCnt--;
  }

  /* Loop unrolling: Compute remaining taps */
  tapCnt = (numTaps - 1U) & 0x3U;



  /* Initialize tapCnt with number of samples */
  tapCnt = (numTaps - 1U);



  while (tapCnt > 0U)
  {
    *pStateCurnt++ = *pState++;

    /* Decrement loop counter */
    tapCnt--;
  }
}
