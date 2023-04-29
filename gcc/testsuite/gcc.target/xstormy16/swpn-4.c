/* { dg-do compile } */
/* { dg-options "-O2" } */

short ior_abc(short x) { return (x&0xff00) | ((x<<4)&0xf0) | ((x>>4)&0x0f); }
short ior_acb(short x) { return (x&0xff00) | ((x>>4)&0x0f) | ((x<<4)&0xf0); }
short ior_bac(short x) { return ((x<<4)&0xf0) | (x&0xff00) | ((x>>4)&0x0f); }
short ior_bca(short x) { return ((x<<4)&0xf0) | ((x>>4)&0x0f) | (x&0xff00); }
short ior_cab(short x) { return ((x>>4)&0x0f) | (x&0xff00) | ((x<<4)&0xf0); }
short ior_cba(short x) { return ((x>>4)&0x0f) | ((x<<4)&0xf0) | (x&0xff00); }

short xor_abc(short x) { return (x&0xff00) ^ ((x<<4)&0xf0) ^ ((x>>4)&0x0f); }
short xor_acb(short x) { return (x&0xff00) ^ ((x>>4)&0x0f) ^ ((x<<4)&0xf0); }
short xor_bac(short x) { return ((x<<4)&0xf0) ^ (x&0xff00) ^ ((x>>4)&0x0f); }
short xor_bca(short x) { return ((x<<4)&0xf0) ^ ((x>>4)&0x0f) ^ (x&0xff00); }
short xor_cab(short x) { return ((x>>4)&0x0f) ^ (x&0xff00) ^ ((x<<4)&0xf0); }
short xor_cba(short x) { return ((x>>4)&0x0f) ^ ((x<<4)&0xf0) ^ (x&0xff00); }

short sum_abc(short x) { return (x&0xff00) + ((x<<4)&0xf0) + ((x>>4)&0x0f); }
short sum_acb(short x) { return (x&0xff00) + ((x>>4)&0x0f) + ((x<<4)&0xf0); }
short sum_bac(short x) { return ((x<<4)&0xf0) + (x&0xff00) + ((x>>4)&0x0f); }
short sum_bca(short x) { return ((x<<4)&0xf0) + ((x>>4)&0x0f) + (x&0xff00); }
short sum_cab(short x) { return ((x>>4)&0x0f) + (x&0xff00) + ((x<<4)&0xf0); }
short sum_cba(short x) { return ((x>>4)&0x0f) + ((x<<4)&0xf0) + (x&0xff00); }

/* { dg-final { scan-assembler-times "swpn r2" 18 } } */
