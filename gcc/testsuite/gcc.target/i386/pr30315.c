/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "cmp" 4 } } */

extern void abort (void);
int c;

#define PLUSCC1(T, t, C)	\
T pluscc##t##C (T a, T b)	\
{	\
  T sum = a + b;	\
  if (sum < C)		\
    abort ();		\
  return sum;		\
}
#define PLUSCC(T, t) PLUSCC1(T, t, a) PLUSCC1(T, t, b)

#define INCCC1(T, t, C)	\
T inccc##t##C (T a, T b)	\
{	\
  T sum = a + b;	\
  if (sum < C)		\
    c ++;		\
  return sum;		\
}
#define INCCC(T, t) INCCC1(T, t, a) INCCC1(T, t, b)

#define PLUSCCONLY1(T, t, C)	\
void pluscconly##t##C (T a, T b)	\
{	\
  T sum = a + b;	\
  if (sum < C)		\
    abort ();		\
}
#define PLUSCCONLY(T, t) PLUSCCONLY1(T, t, a) PLUSCCONLY1(T, t, b)

#define MINUSCC(T, t)	\
T minuscc##t (T a, T b)	\
{	\
  T difference = a - b;	\
  if (difference > a)	\
    abort ();		\
  return difference;	\
}

#define DECCC(T, t)	\
T deccc##t (T a, T b)	\
{	\
  T difference = a - b;	\
  if (difference > a)	\
    c --;		\
  return difference;	\
}

#define MINUSCCONLY(T, t)	\
void minuscconly##t (T a, T b)	\
{	\
  T difference = a - b;	\
  if (difference > a)	\
    abort ();		\
}

#define TEST(T, t)	\
  PLUSCC(T, t)		\
  PLUSCCONLY(T, t)	\
  INCCC(T, t)		\
  MINUSCC(T, t)		\
  MINUSCCONLY(T, t)	\
  DECCC(T, t)

TEST (unsigned long,  l)
TEST (unsigned int,   i)
TEST (unsigned short, s) 
TEST (unsigned char,  c)

#define PLUSCCZEXT(C)	\
unsigned long pluscczext##C (unsigned int a, unsigned int b)	\
{	\
  unsigned int sum = a + b;	\
  if (sum < C)		\
    abort ();		\
  return sum;		\
}

PLUSCCZEXT(a)
PLUSCCZEXT(b)

#define MINUSCCZEXT	\
unsigned long minuscczext (unsigned int a, unsigned int b)	\
{	\
  unsigned int difference = a - b;	\
  if (difference > a)		\
    abort ();			\
  return difference;		\
}

MINUSCCZEXT
