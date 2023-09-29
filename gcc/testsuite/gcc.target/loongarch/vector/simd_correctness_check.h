#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ASSERTEQ_64(line, ref, res)                                           \
  do                                                                          \
    {                                                                         \
      int fail = 0;                                                           \
      for (size_t i = 0; i < sizeof (res) / sizeof (res[0]); ++i)             \
        {                                                                     \
          long *temp_ref = &ref[i], *temp_res = &res[i];                      \
          if (abs (*temp_ref - *temp_res) > 0)                                \
            {                                                                 \
              printf (" error: %s at line %ld , expected " #ref               \
                      "[%ld]:0x%lx, got: 0x%lx\n",                            \
                      __FILE__, line, i, *temp_ref, *temp_res);               \
              fail = 1;                                                       \
            }                                                                 \
        }                                                                     \
      if (fail == 1)                                                          \
        abort ();                                                             \
    }                                                                         \
  while (0)

#define ASSERTEQ_32(line, ref, res)                                           \
  do                                                                          \
    {                                                                         \
      int fail = 0;                                                           \
      for (size_t i = 0; i < sizeof (res) / sizeof (res[0]); ++i)             \
        {                                                                     \
          int *temp_ref = &ref[i], *temp_res = &res[i];                       \
          if (abs (*temp_ref - *temp_res) > 0)                                \
            {                                                                 \
              printf (" error: %s at line %ld , expected " #ref               \
                      "[%ld]:0x%x, got: 0x%x\n",                              \
                      __FILE__, line, i, *temp_ref, *temp_res);               \
              fail = 1;                                                       \
            }                                                                 \
        }                                                                     \
      if (fail == 1)                                                          \
        abort ();                                                             \
    }                                                                         \
  while (0)

#define ASSERTEQ_int(line, ref, res)                                          \
  do                                                                          \
    {                                                                         \
      if (ref != res)                                                         \
        {                                                                     \
          printf (" error: %s at line %ld , expected %d, got %d\n", __FILE__, \
                  line, ref, res);                                            \
        }                                                                     \
    }                                                                         \
  while (0)
