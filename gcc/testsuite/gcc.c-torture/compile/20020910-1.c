/* The bit-field below would have a problem if __INT_MAX__ is too
   small.  */
#if __INT_MAX__ < 2147483647
int a;
#else
unsigned int  x0  = 0;

typedef struct {
  unsigned int  field1 : 20;
  unsigned int  field2 : 12;
} XX;

static XX yy;

static void foo (void)
{
  yy.field1 = (unsigned int ) (&x0);
}
#endif
