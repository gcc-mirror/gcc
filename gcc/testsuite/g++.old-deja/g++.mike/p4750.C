// { dg-do assemble { target native } }
// { dg-options "-fpic -pedantic-errors -S" }
// prms-id: 4750

extern const int FRAME_VEC_MAX;

const int FRAME_VEC_MAX     = 12;
int frame_vec_sizes[FRAME_VEC_MAX+1] = {
        0, 1, 3, 3, 6, 6, 6, 9, 9, 9, 12, 12, 12
};
