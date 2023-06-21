/* { dg-do compile } */
/* { dg-require-profiling "-fprofile-generate" } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O3 -fprofile-generate -fPIC -fno-semantic-interposition" } */

int PyUnicode_FindChar_i;
int PyUnicode_FindChar()
{
  while (PyUnicode_FindChar_i)
    if (PyUnicode_FindChar())
      break;
}
