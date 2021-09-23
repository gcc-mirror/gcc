/* PR target/100760
   This was triggering an ICE with "maximum number of generated reload
   insns per insn achieved (90)" when compiled with -mmsa -mloongson-mmi. */

/* { dg-do compile } */
/* { dg-options "-mmsa -mloongson-mmi" } */

typedef __INT32_TYPE__ int32_t;
typedef int32_t a __attribute__((__vector_size__(8)));
void b() { a x = (a){1, 1}; }
