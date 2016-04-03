/* { dg-additional-options "-std=gnu99 -fpic" }  */
/* { dg-do compile }  */

typedef unsigned long VALUE;
typedef unsigned long ID;

typedef struct rb_callable_method_entry_struct
{
  ID called_id;
  const VALUE owner;
} rb_callable_method_entry_t;

typedef struct rb_iseq_struct rb_iseq_t;

struct __jmp_buf_tag { int xx; };
typedef struct __jmp_buf_tag jmp_buf[1];

struct rb_iseq_struct
{
  const struct iseq_catch_table *catch_table;
};

typedef struct rb_control_frame_struct
{
  const VALUE *pc;
  VALUE *sp;
  const rb_iseq_t *iseq;
  VALUE flag;
  VALUE *ep;
} rb_control_frame_t;

typedef jmp_buf rb_jmpbuf_t;
struct rb_vm_tag
{
  rb_jmpbuf_t buf;
}rb_ensure_list_t;

typedef struct rb_thread_struct
{
  rb_control_frame_t *cfp;
  struct rb_vm_tag *tag;
} rb_thread_t;

struct iseq_catch_table_entry
{
  const rb_iseq_t *iseq;
};

struct iseq_catch_table
{
  unsigned int size;
};

extern unsigned long long __sdt_unsp;
extern unsigned short ruby_cmethod__return_semaphore;

struct ruby_dtrace_method_hook_args
{
  const char *classname;
  const char *methodname;
  const char *filename;
  int line_no;
};

int ruby_th_dtrace_setup(rb_thread_t *th, VALUE klass, ID id, struct ruby_dtrace_method_hook_args *args);
int rb_threadptr_tag_state (rb_thread_t *th);
VALUE vm_exec_core (rb_thread_t *th, VALUE initial);
const rb_callable_method_entry_t *rb_vm_frame_method_entry (const rb_control_frame_t *cfp);

struct vm_throw_data;
const rb_control_frame_t * THROW_DATA_CATCH_FRAME(const struct vm_throw_data *obj);
rb_control_frame_t * vm_push_frame(rb_thread_t *th, const rb_iseq_t *iseq, VALUE type, VALUE self, VALUE specval, VALUE cref_or_me, const VALUE *pc, VALUE *sp, int local_size, int stack_max);


VALUE vm_exec(rb_thread_t *th)
{
  int state;
  VALUE result;
  VALUE initial = 0;
  struct vm_throw_data *err;
  rb_thread_t * const _th = (th);
  struct rb_vm_tag _tag;

  if ((state = (__builtin_setjmp((_tag.buf)) ? rb_threadptr_tag_state((_th)) : ((void)(_th->tag = &_tag), 0))) == 0)
  {
    result = vm_exec_core(th, initial);
  }
  else
  {
    unsigned int i;
    const struct iseq_catch_table_entry *entry;
    const struct iseq_catch_table *ct;
    unsigned long epc, cont_pc, cont_sp;
    const rb_iseq_t *catch_iseq;
    rb_control_frame_t *cfp;
    const rb_control_frame_t *escape_cfp;

    while (th->cfp->pc == 0 || th->cfp->iseq == 0)
    {
      if (ruby_cmethod__return_semaphore)
      {
        struct ruby_dtrace_method_hook_args args;
        if (ruby_th_dtrace_setup(th, rb_vm_frame_method_entry(th->cfp)->owner, rb_vm_frame_method_entry(th->cfp)->called_id, &args))
        {
          __asm__ __volatile__ (
             ".asciz \"%n[_SDT_S1]@%[_SDT_A1] %n[_SDT_S2]@%[_SDT_A2] %n[_SDT_S3]@%[_SDT_A3] %n[_SDT_S4]@%[_SDT_A4]\"\n"
             :
             : [_SDT_S1] "n" (((!__extension__ (__builtin_constant_p ((((unsigned long long) (__typeof (__builtin_choose_expr (((__builtin_classify_type ( args.classname ) + 3) & -4) == 4, ( args.classname ), 0U))) __sdt_unsp) & ((unsigned long long)1 << (sizeof (unsigned long long) * 8 - 1))) == 0) || (__typeof (__builtin_choose_expr (((__builtin_classify_type ( args.classname ) + 3) & -4) == 4, ( args.classname ), 0U))) -1 > (__typeof (__builtin_choose_expr (((__builtin_classify_type ( args.classname ) + 3) & -4) == 4, ( args.classname ), 0U))) 0)) ? 1 : -1) * (int) ((__builtin_classify_type ( args.classname ) == 14 || __builtin_classify_type ( args.classname ) == 5) ? sizeof (void *) : sizeof ( args.classname ))),
               [_SDT_A1] "nor" (( args.classname )),
               [_SDT_S2] "n" (((!__extension__ (__builtin_constant_p ((((unsigned long long) (__typeof (__builtin_choose_expr (((__builtin_classify_type ( args.methodname ) + 3) & -4) == 4, ( args.methodname ), 0U))) __sdt_unsp) & ((unsigned long long)1 << (sizeof (unsigned long long) * 8 - 1))) == 0) || (__typeof (__builtin_choose_expr (((__builtin_classify_type ( args.methodname ) + 3) & -4) == 4, ( args.methodname ), 0U))) -1 > (__typeof (__builtin_choose_expr (((__builtin_classify_type ( args.methodname ) + 3) & -4) == 4, ( args.methodname ), 0U))) 0)) ? 1 : -1) * (int) ((__builtin_classify_type ( args.methodname ) == 14 || __builtin_classify_type ( args.methodname ) == 5) ? sizeof (void *) : sizeof ( args.methodname ))),
               [_SDT_A2] "nor" (( args.methodname )),
               [_SDT_S3] "n" (((!__extension__ (__builtin_constant_p ((((unsigned long long) (__typeof (__builtin_choose_expr (((__builtin_classify_type ( args.filename ) + 3) & -4) == 4, ( args.filename ), 0U))) __sdt_unsp) & ((unsigned long long)1 << (sizeof (unsigned long long) * 8 - 1))) == 0) || (__typeof (__builtin_choose_expr (((__builtin_classify_type ( args.filename ) + 3) & -4) == 4, ( args.filename ), 0U))) -1 > (__typeof (__builtin_choose_expr (((__builtin_classify_type ( args.filename ) + 3) & -4) == 4, ( args.filename ), 0U))) 0)) ? 1 : -1) * (int) ((__builtin_classify_type ( args.filename ) == 14 || __builtin_classify_type ( args.filename ) == 5) ? sizeof (void *) : sizeof ( args.filename ))),
               [_SDT_A3] "nor" (( args.filename )),
               [_SDT_S4] "n" (((!__extension__ (__builtin_constant_p ((((unsigned long long) (__typeof (__builtin_choose_expr (((__builtin_classify_type ( args.line_no ) + 3) & -4) == 4, ( args.line_no ), 0U))) __sdt_unsp) & ((unsigned long long)1 << (sizeof (unsigned long long) * 8 - 1))) == 0) || (__typeof (__builtin_choose_expr (((__builtin_classify_type ( args.line_no ) + 3) & -4) == 4, ( args.line_no ), 0U))) -1 > (__typeof (__builtin_choose_expr (((__builtin_classify_type ( args.line_no ) + 3) & -4) == 4, ( args.line_no ), 0U))) 0)) ? 1 : -1) * (int) ((__builtin_classify_type ( args.line_no ) == 14 || __builtin_classify_type ( args.line_no ) == 5) ? sizeof (void *) : sizeof ( args.line_no ))),
               [_SDT_A4] "nor" (( args.line_no ))
          );
        }
      }
    }

    if (cfp == escape_cfp && !(((cfp)->flag & 0x0200) != 0))
      catch_iseq = entry->iseq;

    if (state == 6)
    {
      escape_cfp = THROW_DATA_CATCH_FRAME(err);

      if (ct)
        for (i = 0; i < ct->size; i++) { }
    }
    else
      ct = cfp->iseq->catch_table;

    vm_push_frame(th, catch_iseq, 0xb1, 0, 1, 0, 0, 0, 5, 123);
  }
}
