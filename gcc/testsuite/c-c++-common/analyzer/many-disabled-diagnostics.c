/* { dg-additional-options "-Wno-analyzer-double-free" } */

#define DOUBLE_FREE()				\
  do {						\
    void *p = __builtin_malloc (1024);		\
    __builtin_free (p);				\
    __builtin_free (p);				\
  } while (0)

#define DOUBLE_FREE_x_10()			\
  do {						\
    DOUBLE_FREE();				\
    DOUBLE_FREE();				\
    DOUBLE_FREE();				\
    DOUBLE_FREE();				\
    DOUBLE_FREE();				\
    DOUBLE_FREE();				\
    DOUBLE_FREE();				\
    DOUBLE_FREE();				\
    DOUBLE_FREE();				\
    DOUBLE_FREE();				\
  } while (0)

#define DOUBLE_FREE_x_100()			\
  do {						\
    DOUBLE_FREE_x_10();				\
    DOUBLE_FREE_x_10();				\
    DOUBLE_FREE_x_10();				\
    DOUBLE_FREE_x_10();				\
    DOUBLE_FREE_x_10();				\
    DOUBLE_FREE_x_10();				\
    DOUBLE_FREE_x_10();				\
    DOUBLE_FREE_x_10();				\
    DOUBLE_FREE_x_10();				\
    DOUBLE_FREE_x_10();				\
  } while (0)

#define DOUBLE_FREE_x_1000()			\
  do {						\
    DOUBLE_FREE_x_100();			\
    DOUBLE_FREE_x_100();			\
    DOUBLE_FREE_x_100();			\
    DOUBLE_FREE_x_100();			\
    DOUBLE_FREE_x_100();			\
    DOUBLE_FREE_x_100();			\
    DOUBLE_FREE_x_100();			\
    DOUBLE_FREE_x_100();			\
    DOUBLE_FREE_x_100();			\
    DOUBLE_FREE_x_100();			\
  } while (0)

void test_1 (void)
{
  DOUBLE_FREE_x_1000 (); 
}
