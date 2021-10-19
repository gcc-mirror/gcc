/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vpunpcklqdq" 1 } } */

typedef _Float16 v8hf __attribute__((vector_size (16)));
typedef _Float16 v4hf __attribute__((vector_size (8)));

v8hf foov (v4hf a, v4hf b)
{
    return __builtin_shufflevector (a, b, 0, 1, 2, 3, 4, 5, 6, 7);
}

v8hf foov2 (v4hf a)
{
    return __builtin_shufflevector (a, (v4hf){0}, 0, 1, 2, 3, 4, 5, 6, 7);
}
