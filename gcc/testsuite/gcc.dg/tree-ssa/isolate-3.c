/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-isolate-paths -fdelete-null-pointer-checks -Wnull-dereference" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */


typedef __SIZE_TYPE__ size_t;
extern void *memset (void *__s, int __c, size_t __n)
  __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__nonnull__ (1)));
struct rtx_def;
typedef struct rtx_def *rtx;
typedef struct VEC_rtx_base

{
  unsigned num;
  unsigned alloc;
  rtx vec[1];
} VEC_rtx_base;
static __inline__ rtx *
VEC_rtx_base_address (VEC_rtx_base * vec_)
{
  return vec_ ? vec_->vec : 0;
}
typedef struct VEC_rtx_gc
{
  VEC_rtx_base base;
} VEC_rtx_gc;

static __inline__ void
VEC_rtx_gc_safe_grow (VEC_rtx_gc ** vec_, int size_, const char *file_,
                      unsigned line_, const char *function_)
{
  ((*vec_) ? &(*vec_)->base : 0)->num = size_; /* { dg-warning "null pointer dereference" } */
} 

static __inline__ void
VEC_rtx_gc_safe_grow_cleared (VEC_rtx_gc ** vec_, int size_,
                              const char *file_, unsigned line_,
                              const char *function_, int oldsize)
{
  VEC_rtx_gc_safe_grow (vec_, size_, file_, line_, function_);
  memset (&(VEC_rtx_base_address ((*vec_) ? &(*vec_)->base : 0))[oldsize], 0,
          sizeof (rtx) * (size_ - oldsize));
}

static VEC_rtx_gc *reg_base_value;
unsigned int max_reg_num (void);
int arf (void);
void
init_alias_analysis (void)
{
  unsigned int maxreg = max_reg_num ();
  (VEC_rtx_gc_safe_grow_cleared
   (&(reg_base_value), maxreg, "../../../gcc-4.6.0/gcc/alias.c", 2755,
    __FUNCTION__, arf ()));
}



/* This is an example of how a NULL pointer dereference can show up
   without a PHI.  Note VEC_rtx_gcc_safe_grow.  If an earlier pass
   (such as VRP) isolates the NULL path for some reason or another
   we end up with an explicit NULL dereference in the IL.  Yes, it
   started with a PHI, but by the time the path isolation code runs
   its explicit in the IL.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "isolate-paths"} } */


