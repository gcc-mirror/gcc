// { dg-do run }
// { dg-options "-O2" }

#define radix 10
__INT32_TYPE__ numDigits(__UINT64_TYPE__ value)
{
     __INT32_TYPE__ n = 1;
     while (value > __UINT32_MAX__)
     {
        n += 4;
        value /= radix * radix * radix * radix;
     }
     __UINT32_TYPE__ v = (__UINT32_TYPE__)value;
     while (1)
     {
         if (v < radix)
             return n;
         if (v < radix * radix)
             return n + 1;
         if (v < radix * radix * radix)
             return n + 2;
         if (v < radix * radix * radix * radix)
             return n + 3;
         n += 4;
         v /= radix * radix * radix * radix;
     }
}

int main()
{
    if (numDigits(__UINT64_MAX__) != 20)
        __builtin_abort();
}
