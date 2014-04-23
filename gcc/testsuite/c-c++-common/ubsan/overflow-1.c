/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow -fno-sanitize-recover" } */

#ifndef ASM1
# define ASM1(a) /* Nothing */
#endif
#ifndef ASM2
# define ASM2(a, b) /* Nothing */
#endif

#define CHECK(A, B) ({ if ((A) != (B)) __builtin_abort (); })

#define FN1(T1, T2, OP)				\
  ({						\
    T1 a = 14;					\
    T2 b = 9;					\
    ASM2 (a, b);				\
    a OP b;					\
  })

#define FN2(T, OP)				\
  ({						\
    T a = 14;					\
    ASM1 (a);					\
    a OP 7;					\
  })

#define FN3(T1, T2, OP)				\
  ({						\
    T1 a = 4;					\
    T2 b = 1;					\
    ASM2 (a, b);				\
    ~a OP b;					\
  })

#define FN4(T1, T2, OP)				\
  ({						\
    T1 a = 4;					\
    T2 b = 1;					\
    ASM2 (a, b);				\
    a OP ~b;					\
  })

#define FN5(T)					\
  ({						\
    T a = 77;					\
    ASM1 (a);					\
    -a;						\
  })

int
main (void)
{
  CHECK (FN1 (char, char, +), 23);
  CHECK (FN1 (char, char, -), 5);
  CHECK (FN1 (char, char, *), 126);
  CHECK (FN1 (unsigned char, unsigned char, +), 23);
  CHECK (FN1 (unsigned char, unsigned char, -), 5);
  CHECK (FN1 (unsigned char, unsigned char, *), 126);
  CHECK (FN1 (short, short, +), 23);
  CHECK (FN1 (short, short, -), 5);
  CHECK (FN1 (short, short, *), 126);
  CHECK (FN1 (unsigned short, unsigned short, +), 23);
  CHECK (FN1 (unsigned short, unsigned short, -), 5);
  CHECK (FN1 (unsigned short, unsigned short, *), 126);
  CHECK (FN1 (int, int, +), 23);
  CHECK (FN1 (int, int, -), 5);
  CHECK (FN1 (int, int, *), 126);
  CHECK (FN1 (unsigned int, unsigned int, +), 23);
  CHECK (FN1 (unsigned int, unsigned int, -), 5);
  CHECK (FN1 (unsigned int, unsigned int, *), 126);
  CHECK (FN1 (long int, long int, +), 23);
  CHECK (FN1 (long int, long int, -), 5);
  CHECK (FN1 (long int, long int, *), 126);
  CHECK (FN1 (unsigned long int, unsigned long int, +), 23);
  CHECK (FN1 (unsigned long int, unsigned long int, -), 5);
  CHECK (FN1 (unsigned long int, unsigned long int, *), 126);
  CHECK (FN1 (long long int, long int, +), 23);
  CHECK (FN1 (long long int, long int, -), 5);
  CHECK (FN1 (long long int, long int, *), 126);
  CHECK (FN1 (unsigned long long int, unsigned long long int, +), 23);
  CHECK (FN1 (unsigned long long int, unsigned long long int, -), 5);
  CHECK (FN1 (unsigned long long int, unsigned long long int, *), 126);
  CHECK (FN1 (int, unsigned char, +), 23);
  CHECK (FN1 (int, unsigned char, -), 5);
  CHECK (FN1 (int, unsigned char, *), 126);
  CHECK (FN1 (unsigned char, int, +), 23);
  CHECK (FN1 (unsigned char, int, -), 5);
  CHECK (FN1 (unsigned char, int, *), 126);
  CHECK (FN1 (int, long int, +), 23);
  CHECK (FN1 (int, long int, -), 5);
  CHECK (FN1 (int, long int, *), 126);
  CHECK (FN1 (long int, int, +), 23);
  CHECK (FN1 (long int, int, -), 5);
  CHECK (FN1 (long int, int, *), 126);
  CHECK (FN1 (unsigned int, int, +), 23);
  CHECK (FN1 (unsigned int, int, -), 5);
  CHECK (FN1 (unsigned int, int, *), 126);
  CHECK (FN1 (int, unsigned int, +), 23);
  CHECK (FN1 (int, unsigned int, -), 5);
  CHECK (FN1 (int, unsigned int, *), 126);
  CHECK (FN1 (unsigned long int, int, +), 23);
  CHECK (FN1 (unsigned long int, int, -), 5);
  CHECK (FN1 (unsigned long int, int, *), 126);
  CHECK (FN1 (int, unsigned long int, +), 23);
  CHECK (FN1 (int, unsigned long int, -), 5);
  CHECK (FN1 (int, unsigned long int, *), 126);

  CHECK (FN2 (char, +), 21);
  CHECK (FN2 (char, -), 7);
  CHECK (FN2 (char, *), 98);
  CHECK (FN2 (unsigned char, +), 21);
  CHECK (FN2 (unsigned char, -), 7);
  CHECK (FN2 (unsigned char, *), 98);
  CHECK (FN2 (short, +), 21);
  CHECK (FN2 (short, -), 7);
  CHECK (FN2 (short, *), 98);
  CHECK (FN2 (unsigned short, +), 21);
  CHECK (FN2 (unsigned short, -), 7);
  CHECK (FN2 (unsigned short, *), 98);
  CHECK (FN2 (int, +), 21);
  CHECK (FN2 (int, -), 7);
  CHECK (FN2 (int, *), 98);
  CHECK (FN2 (unsigned int, +), 21);
  CHECK (FN2 (unsigned int, -), 7);
  CHECK (FN2 (unsigned int, *), 98);
  CHECK (FN2 (long int, +), 21);
  CHECK (FN2 (long int, -), 7);
  CHECK (FN2 (long int, *), 98);
  CHECK (FN2 (unsigned long int, +), 21);
  CHECK (FN2 (unsigned long int, -), 7);
  CHECK (FN2 (unsigned long int, *), 98);
  CHECK (FN2 (long long int, +), 21);
  CHECK (FN2 (long long int, -), 7);
  CHECK (FN2 (long long int, *), 98);
  CHECK (FN2 (unsigned long long int, +), 21);
  CHECK (FN2 (unsigned long long int, -), 7);
  CHECK (FN2 (unsigned long long int, *), 98);

  CHECK (FN3 (char, char, +), -4);
  CHECK (FN3 (char, char, -), -6);
  CHECK (FN3 (char, char, *), -5);
  CHECK (FN3 (unsigned char, unsigned char, +), -4);
  CHECK (FN3 (unsigned char, unsigned char, -), -6);
  CHECK (FN3 (unsigned char, unsigned char, *), -5);
  CHECK (FN3 (short, short, +), -4);
  CHECK (FN3 (short, short, -), -6);
  CHECK (FN3 (short, short, *), -5);
  CHECK (FN3 (unsigned short, unsigned short, +), -4);
  CHECK (FN3 (unsigned short, unsigned short, -), -6);
  CHECK (FN3 (unsigned short, unsigned short, *), -5);
  CHECK (FN3 (int, int, +), -4);
  CHECK (FN3 (int, int, -), -6);
  CHECK (FN3 (int, int, *), -5);
  CHECK (FN3 (unsigned int, unsigned int, +), -4);
  CHECK (FN3 (unsigned int, unsigned int, -), -6);
  CHECK (FN3 (unsigned int, unsigned int, *), -5);
  CHECK (FN3 (long int, long int, +), -4);
  CHECK (FN3 (long int, long int, -), -6);
  CHECK (FN3 (long int, long int, *), -5);
  CHECK (FN3 (unsigned long int, unsigned long int, +), -4);
  CHECK (FN3 (unsigned long int, unsigned long int, -), -6);
  CHECK (FN3 (unsigned long int, unsigned long int, *), -5);
  CHECK (FN3 (long long int, long int, +), -4);
  CHECK (FN3 (long long int, long int, -), -6);
  CHECK (FN3 (long long int, long int, *), -5);
  CHECK (FN3 (unsigned long long int, unsigned long long int, +), -4);
  CHECK (FN3 (unsigned long long int, unsigned long long int, -), -6);
  CHECK (FN3 (unsigned long long int, unsigned long long int, *), -5);
  CHECK (FN3 (int, unsigned char, +), -4);
  CHECK (FN3 (int, unsigned char, -), -6);
  CHECK (FN3 (int, unsigned char, *), -5);
  CHECK (FN3 (unsigned char, int, +), -4);
  CHECK (FN3 (unsigned char, int, -), -6);
  CHECK (FN3 (unsigned char, int, *), -5);
  CHECK (FN3 (int, long int, +), -4);
  CHECK (FN3 (int, long int, -), -6);
  CHECK (FN3 (int, long int, *), -5);
  CHECK (FN3 (long int, int, +), -4);
  CHECK (FN3 (long int, int, -), -6);
  CHECK (FN3 (long int, int, *), -5);
  CHECK (FN3 (unsigned int, int, +), -4);
  CHECK (FN3 (unsigned int, int, -), -6);
  CHECK (FN3 (unsigned int, int, *), -5);
  CHECK (FN3 (int, unsigned int, +), -4);
  CHECK (FN3 (int, unsigned int, -), -6);
  CHECK (FN3 (int, unsigned int, *), -5);
  CHECK (FN3 (unsigned long int, int, +), -4);
  CHECK (FN3 (unsigned long int, int, -), -6);
  CHECK (FN3 (unsigned long int, int, *), -5);
  CHECK (FN3 (int, unsigned long int, +), -4);
  CHECK (FN3 (int, unsigned long int, -), -6);
  CHECK (FN3 (int, unsigned long int, *), -5);

  CHECK (FN4 (char, char, +), 2);
  CHECK (FN4 (char, char, -), 6);
  CHECK (FN4 (char, char, *), -8);
  CHECK (FN4 (unsigned char, unsigned char, +), 2);
  CHECK (FN4 (unsigned char, unsigned char, -), 6);
  CHECK (FN4 (unsigned char, unsigned char, *), -8);
  CHECK (FN4 (short, short, +), 2);
  CHECK (FN4 (short, short, -), 6);
  CHECK (FN4 (short, short, *), -8);
  CHECK (FN4 (unsigned short, unsigned short, +), 2);
  CHECK (FN4 (unsigned short, unsigned short, -), 6);
  CHECK (FN4 (unsigned short, unsigned short, *), -8);
  CHECK (FN4 (int, int, +), 2);
  CHECK (FN4 (int, int, -), 6);
  CHECK (FN4 (int, int, *), -8);
  CHECK (FN4 (unsigned int, unsigned int, +), 2);
  CHECK (FN4 (unsigned int, unsigned int, -), 6);
  CHECK (FN4 (unsigned int, unsigned int, *), -8);
  CHECK (FN4 (long int, long int, +), 2);
  CHECK (FN4 (long int, long int, -), 6);
  CHECK (FN4 (long int, long int, *), -8);
  CHECK (FN4 (unsigned long int, unsigned long int, +), 2);
  CHECK (FN4 (unsigned long int, unsigned long int, -), 6);
  CHECK (FN4 (unsigned long int, unsigned long int, *), -8);
  CHECK (FN4 (long long int, long int, +), 2);
  CHECK (FN4 (long long int, long int, -), 6);
  CHECK (FN4 (long long int, long int, *), -8);
  CHECK (FN4 (unsigned long long int, unsigned long long int, +), 2);
  CHECK (FN4 (unsigned long long int, unsigned long long int, -), 6);
  CHECK (FN4 (unsigned long long int, unsigned long long int, *), -8);
  CHECK (FN4 (int, unsigned char, +), 2);
  CHECK (FN4 (int, unsigned char, -), 6);
  CHECK (FN4 (int, unsigned char, *), -8);
  CHECK (FN4 (unsigned char, int, +), 2);
  CHECK (FN4 (unsigned char, int, -), 6);
  CHECK (FN4 (unsigned char, int, *), -8);
  CHECK (FN4 (int, long int, +), 2);
  CHECK (FN4 (int, long int, -), 6);
  CHECK (FN4 (int, long int, *), -8);
  CHECK (FN4 (long int, int, +), 2);
  CHECK (FN4 (long int, int, -), 6);
  CHECK (FN4 (long int, int, *), -8);
  CHECK (FN4 (unsigned int, int, +), 2);
  CHECK (FN4 (unsigned int, int, -), 6);
  CHECK (FN4 (unsigned int, int, *), -8);
  CHECK (FN4 (int, unsigned int, +), 2);
  CHECK (FN4 (int, unsigned int, -), 6);
  CHECK (FN4 (int, unsigned int, *), -8);
  CHECK (FN4 (unsigned long int, int, +), 2);
  CHECK (FN4 (unsigned long int, int, -), 6);
  CHECK (FN4 (unsigned long int, int, *), -8);
  CHECK (FN4 (int, unsigned long int, +), 2);
  CHECK (FN4 (int, unsigned long int, -), 6);
  CHECK (FN4 (int, unsigned long int, *), -8);

  CHECK (FN5 (char), -77);
  CHECK (FN5 (unsigned char), -77);
  CHECK (FN5 (short), -77);
  CHECK (FN5 (unsigned short), -77);
  CHECK (FN5 (int), -77);
  CHECK (FN5 (unsigned int), -77);
  CHECK (FN5 (long int), -77);
  CHECK (FN5 (unsigned long int), -77);
  CHECK (FN5 (long long int), -77);
  CHECK (FN5 (unsigned long long int), -77);
  return 0;
}
