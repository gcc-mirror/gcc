#define CMP_VI(NAME, TYPE, NITERS, OP, IMM)                                    \
  void NAME (int n, TYPE **__restrict a)                                       \
  {                                                                            \
    int b;                                                                     \
    int c;                                                                     \
    int d;                                                                     \
    for (b = 0; b < NITERS; b++)                                               \
      for (int e = 8; e > 0; e--)                                              \
	a[b][e] = a[b][e] OP IMM;                                              \
  }
  
