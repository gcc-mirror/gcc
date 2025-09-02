/* { dg-do compile } */
/* { dg-options "-O2" } */
/* PR target/121385 */

#pragma GCC target "+cmpbr"

struct DWstruct {
    long low, high;
};
typedef union {
    struct DWstruct s;
    __int128 ll;
} DWunion;
__int128 f(__int128 u) {
    if (u >> 64 == 0)
     {
        __int128 t = (__int128)(unsigned long )u * 2;
        DWunion ww;
        ww.ll = t;
        ww.s.high -= 1;
        if (ww.s.high >= 0)
            return ww.ll;
    }
  return 0;
}
