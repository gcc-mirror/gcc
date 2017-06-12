/* { dg-do compile } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */

void foo(unsigned char * __seg_gs *pointer_gs) {
        pointer_gs[5] = 0;
}
