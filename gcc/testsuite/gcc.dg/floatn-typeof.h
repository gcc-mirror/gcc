/* Tests for _FloatN / _FloatNx types: test types of constants.
   Before including this file, define WIDTH as the value N; define EXT
   to 1 for _FloatNx and 0 for _FloatN.  */

#define CONCATX(X, Y) X ## Y
#define CONCAT(X, Y) CONCATX (X, Y)
#define CONCAT3(X, Y, Z) CONCAT (CONCAT (X, Y), Z)
#define CONCAT4(W, X, Y, Z) CONCAT (CONCAT (CONCAT (W, X), Y), Z)

#if EXT
# define TYPE CONCAT3 (_Float, WIDTH, x)
# define CST(C) CONCAT4 (C, f, WIDTH, x)
# define CSTU(C) CONCAT4 (C, F, WIDTH, x)
#else
# define TYPE CONCAT (_Float, WIDTH)
# define CST(C) CONCAT3 (C, f, WIDTH)
# define CSTU(C) CONCAT3 (C, F, WIDTH)
#endif

extern TYPE test_type;
extern __typeof (CST (1.0)) test_type;
extern __typeof (CSTU (1.0)) test_type;
