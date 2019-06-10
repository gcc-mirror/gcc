class fixed_wide_int_storage {
public:
  long val[10];
  int len;
  fixed_wide_int_storage ()
    {
      len = sizeof (val) / sizeof (val[0]);
      for (int i = 0; i < len; i++)
	val[i] = i;
    }
};

extern void foo (fixed_wide_int_storage);
extern int record_increment(void);
