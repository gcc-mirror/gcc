/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mno-avx512bw" } */

typedef unsigned int __attribute__((mode(HI), vector_size(16))) v8hi_t;
typedef unsigned int __attribute__((mode(HI), vector_size(32))) v16hi_t;

struct s8hi {
	int i;
	v8hi_t __attribute__((packed)) v;
};
struct s16hi {
	int i;
	v16hi_t __attribute__((packed)) v;
};

void f8hi(struct s8hi*p1, const struct s8hi*p2) {
	p1->v += p2->v;
}

void f16hi(struct s16hi*p1, const struct s16hi*p2) {
	p1->v += p2->v;
}

/* { dg-final { scan-assembler-not "^\[ \t\]*vmovdq\[au\](8|16)" } } */
