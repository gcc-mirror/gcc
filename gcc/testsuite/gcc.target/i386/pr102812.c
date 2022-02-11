/* PR target/102812 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse4 -mno-avx" } */
/* { dg-final { scan-assembler-not "vmovdqa64\t" } } */

typedef _Float16 v8hf __attribute__((__vector_size__ (16)));

v8hf t (_Float16 a)
{
    return (v8hf) {a, 0, 0, 0, 0, 0, 0, 0};
}
