/* { dg-options "-std=gnu99" } */

/* C99 6.5.3 Unary operators.  */

extern void abort (void);

#define AUTO_INCREASE_DECREASE(TYPE,SUFFIX)   		\
do                                            		\
{                                             		\
  _Decimal##TYPE in_de_d##TYPE = 0.0##SUFFIX;         	\
  if (in_de_d##TYPE++) abort ();              		\
  if (--in_de_d##TYPE) abort ();              		\
  if (++in_de_d##TYPE == 0.0##SUFFIX) abort (); 	\
  if (in_de_d##TYPE-- == 0.0##SUFFIX) abort (); 	\
} while(0)

#define UNARY_OPERATOR(TYPE,SUFFIX)           		\
do                                            		\
{                                             		\
 _Decimal##TYPE unary_d##TYPE = 1.0##SUFFIX;  		\
 _Decimal##TYPE* unary_dp##TYPE;              		\
 /*  & operator.  */                          		\
 unary_dp##TYPE = &(unary_d##TYPE);           		\
 /*  * operator.  */                          		\
 unary_d##TYPE = *(unary_dp##TYPE);           		\
 /*  - operator.  */                          		\
 unary_d##TYPE = -unary_d##TYPE;              		\
 if (unary_d##TYPE != -1.0##SUFFIX) abort ();         	\
 /*  + operator.  */                          		\
 unary_d##TYPE = +unary_d##TYPE;              		\
 if (unary_d##TYPE != -1.0##SUFFIX) abort ();         	\
 if (!unary_d##TYPE) abort (); /*! operator.  */ 	\
} while (0)

int
main ()
{
  /*  C99 6.5.3.1 Prefix increment and decrement operators. */
  AUTO_INCREASE_DECREASE(32, DF);
  AUTO_INCREASE_DECREASE(64, DD);
  AUTO_INCREASE_DECREASE(128, DL);

  /*  C99 6.5.3 Unary operators.  */
  UNARY_OPERATOR(32, DF);
  UNARY_OPERATOR(64, DD);
  UNARY_OPERATOR(128, DL);

  /*  C99 6.5.3 Unary operators for zero values.  */
  if (- +0.df != -0.df) abort ();
  if (+ -0.df != -0.df) abort ();
  if (- -0.df != +0.df) abort ();

  return 0;
}
