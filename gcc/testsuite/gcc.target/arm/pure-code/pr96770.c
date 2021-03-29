/* { dg-do compile } */
/* { dg-options "-mpure-code" } */

int arr[1000];
int *f4 (void) { return &arr[1]; }

/* For cortex-m0 (thumb-1/v6m), we generate 4 movs with upper/lower:#arr+4.  */
/* { dg-final { scan-assembler-times "arr\\+4" 4 { target { { ! arm_thumb1_movt_ok } && { ! arm_thumb2_ok } } } } } */

/* For cortex-m with movt/movw (thumb-1/v8m.base or thumb-2), we
   generate a movt/movw pair with upper/lower:#arr+4.  */
/* { dg-final { scan-assembler-times "arr\\+4" 2 { target { arm_thumb1_movt_ok || arm_thumb2_ok } } } } */

int *f5 (void) { return &arr[80]; }

/* For cortex-m0 (thumb-1/v6m), we generate 1 ldr from rodata pointer to arr+320.  */
/* { dg-final { scan-assembler-times "arr\\+320" 1 { target { { ! arm_thumb1_movt_ok } && { ! arm_thumb2_ok } } } } } */

/* For cortex-m with movt/movw (thumb-1/v8m.base or thumb-2), we
   generate a movt/movw pair with upper/lower:arr+320.  */
/* { dg-final { scan-assembler-times "arr\\+320" 2 { target { arm_thumb1_movt_ok || arm_thumb2_ok } } } } */
