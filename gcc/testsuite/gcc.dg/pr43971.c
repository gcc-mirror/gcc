/* { dg-do compile } */
/* { dg-options "-Os -fno-delete-null-pointer-checks" } */

union ktime {
    long tv64;
};
typedef union ktime ktime_t;
ktime_t
do_one_initcall(ktime_t rettime, ktime_t calltime)
{
    ktime_t delta;
    delta = ({ (ktime_t){ .tv64 = (rettime).tv64 - (calltime).tv64 }; });
    return delta;
}
