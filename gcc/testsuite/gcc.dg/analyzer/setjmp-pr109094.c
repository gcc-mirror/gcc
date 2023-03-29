/* Reduced from an ICE seen in qemu's target/i386/tcg/translate.c  */

typedef long int __jmp_buf[8];
struct __jmp_buf_tag {
  __jmp_buf __jmpbuf;
};
typedef struct __jmp_buf_tag sigjmp_buf[1];

extern int __sigsetjmp(sigjmp_buf env, int savesigs);
extern void siglongjmp(sigjmp_buf env, int val);

typedef struct DisasContextBase {
  int num_insns;
} DisasContextBase;

typedef struct DisasContext {
  DisasContextBase base;
  sigjmp_buf jmpbuf;
} DisasContext;

extern int translator_ldub(DisasContextBase *base, int);

int advance_pc(DisasContext *s, int num_bytes) {
  if (s->base.num_insns > 1) {
    siglongjmp(s->jmpbuf, 2);
  }
  return 0;
}

static inline int x86_ldub_code(DisasContext *s) {
  return translator_ldub(&s->base, advance_pc(s, 1));
}

static void disas_insn(DisasContext *s) {
  int b;
  __sigsetjmp(s->jmpbuf, 0);
  b = x86_ldub_code(s);
}
