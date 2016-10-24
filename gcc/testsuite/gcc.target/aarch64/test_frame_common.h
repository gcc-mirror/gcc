extern void abort ();

#define CVT(v) ((unsigned char)(v))

static void __attribute__((noinline, noclone))
check_args_8 (int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7,
	      int a8)
{
  if (a0 != 0
      || a1 != 1
      || a2 != 2
      || a3 != 3
      || a4 != 4
      || a5 != 5
      || a6 != 6
      || a7 != 7
      || a8 != 8)
    abort ();
}

static void __attribute__((noinline, noclone))
check_args_24 (int a0, int a1, int a2, int a3, int a4, int a5, int a6, int a7,
	       int a8, int a9, int a10)
{
  if (a0 != 0
      || a1 != 1
      || a2 != 2
      || a3 != 3
      || a4 != 4
      || a5 != 5
      || a6 != 6
      || a7 != 7
      || a8 != 8
      || a9 != 9
      || a10 != 10)
    abort ();
}

void __attribute__ ((noinline, noclone))
initialize_array (unsigned char *a, int len)
{
  int i;

  for (i = 0; i < (len / 2); i++)
    {
      a[i] = i;
      a[len - i - 1] = i;
    }

  return;
}

#define t_frame_pattern(name, local_size, callee_saved)\
int \
name (void)\
{\
  unsigned char a[local_size];\
  initialize_array (a, local_size); \
  __asm__ ("":::callee_saved); \
  if (a[0] != a[local_size - 1] \
      || a[0] != 0) \
    return 0; \
  if (a[local_size / 2 - 1] != a[local_size / 2] \
      || a[local_size / 2 - 1] != CVT (local_size / 2 - 1)) \
    return 0; \
  return 1; \
}

#define t_frame_pattern_outgoing(name, local_size, callee_saved, out_going_num, ...)\
int \
name (void)\
{\
  unsigned char a[local_size];\
  initialize_array (a, local_size); \
  __asm__ ("":::callee_saved); \
  if (a[0] != a[local_size - 1] \
      || a[0] != 0) \
    return 0; \
  if (a[local_size / 2 - 1] != a[local_size / 2] \
      || a[local_size / 2 - 1] != CVT (local_size / 2 - 1)) \
    return 0; \
  check_args_ ## out_going_num (a[0], a[1], a[2], a[3], a[4], a[5], a[6],\
				a[7], __VA_ARGS__); \
  return 1; \
}

#define t_frame_run(name) \
int \
main (int argc, char **argv) \
{\
  if (!name ())\
    abort ();\
  return 0;\
}
