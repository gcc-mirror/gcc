/* PR 17962 */
/* { dg-do compile } */
/* { dg-options "" } */

typedef float v4 __attribute__((vector_size(sizeof(float)*4)));
extern char compile_time_assert[__alignof__(v4) == sizeof(float)*4 ? 1 : -1];
