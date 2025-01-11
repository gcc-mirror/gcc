typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;

// Generates Galois Field  GF(2^15)  =  GF(2)[x] mod (p * GF(2)[x])
#define POLY 0xe125 // 1 + x^2 + x^5 + x^8 + x^13 + x^14 + x^15
#define DEGREE 15

typedef uint16_t poly;

#define MASK(b)    (((poly) 1) << (b))

#ifndef BIT
#define BIT 0
#endif

// Calculate a * b mod p.
poly pprod (poly a, poly b)
{
    const poly mask = MASK (DEGREE);
    poly c = 0;

    while (b)
    {
        // Is bit i of b set?
        if (b & (1u << BIT))
        {
            //__asm ("" : "+r" (c));
            c ^= a;     // yes, then c := c + a * x^i
        }
        a <<= 1;        // a := a*x

        if (a & mask)   // a := a mod p
            a ^= POLY;

        b >>= 1;
    }

    return c;
}

uint32_t pprod32 (uint32_t a, uint32_t b)
{
    const uint32_t mask = MASK (DEGREE);
    uint32_t c = 0;

    while (b)
    {
        // Is bit i of b set?
        if (b & (1u << BIT))
        {
            //__asm ("" : "+r" (c));
            c ^= a;     // yes, then c := c + a * x^i
        }
        a <<= 1;        // a := a*x

        if (a & mask)   // a := a mod p
            a ^= POLY;

        b >>= 1;
    }

    return c;
}
