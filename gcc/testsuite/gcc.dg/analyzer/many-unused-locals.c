struct st
{
  const char *m_filename;
  int m_line;
  const char *m_function;
};

extern void debug (struct st *);

#define TEST_x_1(NAME)							\
  do									\
    {									\
      static struct st NAME = { __FILE__, __LINE__, __func__ };		\
      debug (&NAME);							\
    }									\
  while (0)

#define TEST_x_10(PREFIX)						\
  do									\
    {									\
      TEST_x_1(PREFIX ## _1);						\
      TEST_x_1(PREFIX ## _2);						\
      TEST_x_1(PREFIX ## _3);						\
      TEST_x_1(PREFIX ## _4);						\
      TEST_x_1(PREFIX ## _5);						\
      TEST_x_1(PREFIX ## _6);						\
      TEST_x_1(PREFIX ## _7);						\
      TEST_x_1(PREFIX ## _8);						\
      TEST_x_1(PREFIX ## _9);						\
      TEST_x_1(PREFIX ## _10);						\
    }									\
  while(0)

#define TEST_x_100(PREFIX)						\
  do									\
    {									\
      TEST_x_10(PREFIX ## _1);						\
      TEST_x_10(PREFIX ## _2);						\
      TEST_x_10(PREFIX ## _3);						\
      TEST_x_10(PREFIX ## _4);						\
      TEST_x_10(PREFIX ## _5);						\
      TEST_x_10(PREFIX ## _6);						\
      TEST_x_10(PREFIX ## _7);						\
      TEST_x_10(PREFIX ## _8);						\
      TEST_x_10(PREFIX ## _9);						\
      TEST_x_10(PREFIX ## _10);						\
    }									\
  while(0)

#define TEST_x_1000(PREFIX)						\
  do									\
    {									\
      TEST_x_100(PREFIX ## _1);						\
      TEST_x_100(PREFIX ## _2);						\
      TEST_x_100(PREFIX ## _3);						\
      TEST_x_100(PREFIX ## _4);						\
      TEST_x_100(PREFIX ## _5);						\
      TEST_x_100(PREFIX ## _6);						\
      TEST_x_100(PREFIX ## _7);						\
      TEST_x_100(PREFIX ## _8);						\
      TEST_x_100(PREFIX ## _9);						\
      TEST_x_100(PREFIX ## _10);						\
    }									\
  while(0)

void test_many (void)
{
  TEST_x_1000(s);
}
