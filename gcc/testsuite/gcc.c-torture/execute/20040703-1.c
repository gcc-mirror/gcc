/* PR 16341 */

#define PART_PRECISION (sizeof (cpp_num_part) * 8)

typedef unsigned int cpp_num_part;
typedef struct cpp_num cpp_num;
struct cpp_num
{
   cpp_num_part high;
   cpp_num_part low;
   int unsignedp;  /* True if value should be treated as unsigned.  */
   int overflow;   /* True if the most recent calculation overflowed.  */
};

static  int
num_positive (cpp_num num, unsigned int precision)
{
   if (precision > PART_PRECISION)
     {
       precision -= PART_PRECISION;
       return (num.high & (cpp_num_part) 1 << (precision - 1)) == 0;
     }

   return (num.low & (cpp_num_part) 1 << (precision - 1)) == 0;
}

static cpp_num
num_trim (cpp_num num, unsigned int precision)
{
   if (precision > PART_PRECISION)
     {
       precision -= PART_PRECISION;
       if (precision < PART_PRECISION)
         num.high &= ((cpp_num_part) 1 << precision) - 1;
     }
   else
     {
       if (precision < PART_PRECISION)
         num.low &= ((cpp_num_part) 1 << precision) - 1;
       num.high = 0;
     }

   return num;
}

/* Shift NUM, of width PRECISION, right by N bits.  */
static cpp_num
num_rshift (cpp_num num, unsigned int precision, unsigned int n)
{
   cpp_num_part sign_mask;
   int x = num_positive (num, precision);

   if (num.unsignedp || x)
     sign_mask = 0;
   else
     sign_mask = ~(cpp_num_part) 0;

   if (n >= precision)
     num.high = num.low = sign_mask;
   else
     {
       /* Sign-extend.  */
       if (precision < PART_PRECISION)
         num.high = sign_mask, num.low |= sign_mask << precision;
       else if (precision < 2 * PART_PRECISION)
         num.high |= sign_mask << (precision - PART_PRECISION);

       if (n >= PART_PRECISION)
         {
           n -= PART_PRECISION;
           num.low = num.high;
           num.high = sign_mask;
         }

       if (n)
         {
           num.low = (num.low >> n) | (num.high << (PART_PRECISION - n));
           num.high = (num.high >> n) | (sign_mask << (PART_PRECISION - n));
         }
     }

   num = num_trim (num, precision);
   num.overflow = 0;
   return num;
}
                              #define num_zerop(num) ((num.low | num.high) == 0)
#define num_eq(num1, num2) (num1.low == num2.low && num1.high == num2.high)

cpp_num
num_lshift (cpp_num num, unsigned int precision, unsigned int n)
{
   if (n >= precision)
     {
       num.overflow = !num.unsignedp && !num_zerop (num);
       num.high = num.low = 0;
     }
   else
     {
       cpp_num orig;
       unsigned int m = n;

       orig = num;
       if (m >= PART_PRECISION)
         {
           m -= PART_PRECISION;
           num.high = num.low;
           num.low = 0;
         }
       if (m)
         {
           num.high = (num.high << m) | (num.low >> (PART_PRECISION - m));
           num.low <<= m;
         }
       num = num_trim (num, precision);

       if (num.unsignedp)
         num.overflow = 0;
       else
         {
           cpp_num maybe_orig = num_rshift (num, precision, n);
           num.overflow = !num_eq (orig, maybe_orig);
         }
     }

   return num;
}

unsigned int precision = 64;
unsigned int n = 16;

cpp_num num = { 0, 3, 0, 0 };

int main()
{
   cpp_num res = num_lshift (num, 64, n);

   if (res.low != 0x30000)
     abort ();

   if (res.high != 0)
     abort ();

   if (res.overflow != 0)
     abort ();

   exit (0);
}
