#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ASSERTEQ_64(line, ref, res)                                           \
  do                                                                          \
    {                                                                         \
      int fail = 0;                                                           \
      for (size_t i = 0; i < sizeof (res) / sizeof (res[0]); ++i)             \
        {                                                                     \
          long long *temp_ref = (long long *)&ref[i],                         \
		*temp_res = (long long *)&res[i];			      \
          if (llabs (*temp_ref - *temp_res) > 0)                                \
            {                                                                 \
              printf (" error: %s at line %ld , expected " #ref               \
                      "[%ld]:0x%016lx, got: 0x%016lx\n",                      \
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
          int *temp_ref = (int *)&ref[i], *temp_res = (int *)&res[i];         \
          if (abs (*temp_ref - *temp_res) > 0)                                \
            {                                                                 \
              printf (" error: %s at line %ld , expected " #ref               \
                      "[%ld]:0x%08x, got: 0x%08x\n",                          \
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
          printf (" error: %s at line %ld , expected 0x:%016x",               \
		  "got 0x:%016x\n", __FILE__, line, ref, res);                \
        }                                                                     \
    }                                                                         \
  while (0)
