/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-Os -fno-strict-aliasing -fPIC -mthumb -march=armv7-a -mfpu=vfp3 -mfloat-abi=softfp" } */

extern void foo (float *);

void joint_decode(float* mlt_buffer1, int t) {
    int i;
    float decode_buffer[1060];
    foo(decode_buffer);
    for (i=0; i<10 ; i++) {
        mlt_buffer1[i] = i * decode_buffer[t];
    }
}
