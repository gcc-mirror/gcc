/* PR preprocessor/104147 */
/* { dg-do run } */

#define X(x,y) 	x y
#define STR_(x) #x
#define STR(x) 	STR_(x)
const char *str =
STR(X(Y,Y))
#define Y()
STR(X(Y,Y))
#undef Y
STR(X(Y,Y))
#define Y()
STR(X(Y,Y))
STR(X(Y,
Y))
STR(X(Y
,Y))
;

int
main ()
{
  if (__builtin_strcmp (str, "Y YY YY YY YY YY Y") != 0)
    __builtin_abort ();
  return 0;
}
