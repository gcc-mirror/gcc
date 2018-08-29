/* PR target/80968 */
/* { dg-do compile } */
/* { dg-skip-if "no register windows" { *-*-* } { "-mflat" } { "" } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-mcpu=ultrasparc -O" } */

/* Make sure references to the stack frame do not slip into the delay slot
   of a return instruction.  */

struct crypto_shash {
	unsigned int descsize;
};
struct crypto_shash *tfm;

struct shash_desc {
	struct crypto_shash *tfm;
	unsigned int flags;

	void *__ctx[] __attribute__((aligned(8)));
};

static inline unsigned int crypto_shash_descsize(struct crypto_shash *tfm)
{
	return tfm->descsize;
}

static inline void *shash_desc_ctx(struct shash_desc *desc)
{
	return desc->__ctx;
}

#define SHASH_DESC_ON_STACK(shash, ctx)				  \
	char __##shash##_desc[sizeof(struct shash_desc) +	  \
			      crypto_shash_descsize(ctx)] __attribute__((aligned(8))); \
	struct shash_desc *shash = (struct shash_desc *)__##shash##_desc

extern int crypto_shash_update(struct shash_desc *, const void *, unsigned int);

unsigned int bug(unsigned int crc, const void *address, unsigned int length)
{
	SHASH_DESC_ON_STACK(shash, tfm);
	unsigned int *ctx = (unsigned int *)shash_desc_ctx(shash);
	int err;

	shash->tfm = tfm;
	shash->flags = 0;
	*ctx = crc;

	err = crypto_shash_update(shash, address, length);

	return *ctx;
}
/* { dg-final { scan-assembler "ld\[ \t\]*\\\[%i5\\+8\\\], %i0\n\[^\n\]*return\[ \t\]*%i7\\+8" } } */
