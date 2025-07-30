// PR c++/120778
// { dg-do preprocess }
// { dg-options "-pedantic-errors" }

#if (1, 2)
#define M1 1
#else
#error
#endif
#if 1 ? 2, 3 : 4
#define M2 2
#else
#error
#endif
#if 0 ? 2, 0 : 1
#define M3 3
#else
#error
#endif
#if 0 || (1, 2)
#define M4 4
#else
#error
#endif
#if 1 || (1, 2)
#define M5 5
#else
#error
#endif
#if (1, 2) && 1
#define M6 6
#else
#error
#endif
#if 1 && (1, 2)
#define M7 7
#else
#error
#endif
#if M1 + M2 + M3 + M4 + M5 + M6 + M7 != 28
#error
#endif
