/* PR rtl-optimization/128251 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

extern long double rsp;
struct __attribute__((aligned(32))) m256_2_struct {
    int x1, x2;
};
void check_struct_passing2(struct m256_2_struct ms
                           __attribute__((__unused__))) {

    if (!((unsigned long)&ms.x1 == rsp + 8))
        __builtin_abort();
    if (!((unsigned long)&ms.x2 == rsp + 40))
        __builtin_abort();
}
