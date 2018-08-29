/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mno-avx512bw" } */

typedef unsigned int __attribute__((mode(QI), vector_size(16))) v16qi_t;
typedef unsigned int __attribute__((mode(QI), vector_size(32))) v32qi_t;

struct s16qi {
	int i;
	v16qi_t __attribute__((packed)) v;
};
struct s32qi {
	int i;
	v32qi_t __attribute__((packed)) v;
};

void f16qi(struct s16qi*p1, const struct s16qi*p2) {
	p1->v += p2->v;
}

void f32qi(struct s32qi*p1, const struct s32qi*p2) {
	p1->v += p2->v;
}

/* { dg-final { scan-assembler-not "^\[ \t\]*vmovdq\[au\](8|16)" } } */
