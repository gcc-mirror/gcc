/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */

typedef int int32_t;
typedef unsigned char uint8_t;
typedef unsigned long int uintptr_t;
typedef uint8_t scm_t_uint8;
typedef int32_t scm_t_int32;
typedef uintptr_t scm_t_uintptr;
typedef scm_t_uintptr scm_t_bits;
typedef struct scm_unused_struct {
} *SCM;
enum scm_tc8_tags {
    scm_tc8_flag = 4 + 0x00,   scm_tc8_char = 4 + 0x08,   scm_tc8_unused_0 = 4 + 0x10,   scm_tc8_unused_1 = 4 + 0x18 };
struct __jmp_buf_tag   {
};
typedef struct __jmp_buf_tag jmp_buf[1];
typedef struct scm_t_cell {
} scm_t_cell;
struct scm_prompt_registers {
    jmp_buf regs;
};
enum {
    SCM_VM_APPLY_HOOK,   SCM_VM_PUSH_CONTINUATION_HOOK,   SCM_VM_POP_CONTINUATION_HOOK,   SCM_VM_NEXT_HOOK,   SCM_VM_ABORT_CONTINUATION_HOOK,   SCM_VM_RESTORE_CONTINUATION_HOOK,   SCM_VM_NUM_HOOKS, };
typedef SCM (*scm_t_vm_engine) (SCM vm, SCM program, SCM *argv, int nargs);
struct scm_vm {
    scm_t_uint8 *ip;
    SCM *sp;
    SCM *fp;
    int engine;
    int trace_level;
};
static SCM vm_regular_engine (SCM vm, SCM program, SCM *argv, int nargs) {
}
static SCM vm_debug_engine (SCM vm, SCM program, SCM *argv, int nargs) {
    register scm_t_uint8 *ip ;
    register SCM *sp ;
    register SCM *fp ;
    struct scm_vm *vp = ((struct scm_vm *) ((((scm_t_bits) (0? (*(SCM*)0=((((SCM *)((scm_t_cell *) (((scm_t_bits) (0? (*(SCM*)0=((((vm))))): (((vm)))))))) [((1))]))): (((SCM *)((scm_t_cell *) (((scm_t_bits) (0? (*(SCM*)0=((((vm))))): (((vm)))))))) [((1))]))))));
    static const void **jump_table_pointer = ((void *)0);
    const void **jump_table;
    if (__builtin_expect ((!jump_table_pointer), 0))     {
	jump_table_pointer[0] = &&l_nop;
    }
l_nop:
      {
	SCM *old_sp;
	scm_t_int32 n;
	old_sp = sp;
	sp = (fp - 1) + n;
	if (old_sp < sp)     {
	    while (old_sp < sp)         *++old_sp = ((SCM) ((((((9)) << 8) + scm_tc8_flag))));
	}
	  {
	      { if (__builtin_expect ((vp->trace_level > 0), 0)) { { vp->ip = ip; vp->sp = sp; vp->fp = fp; }; vm_dispatch_hook (vm, SCM_VM_NEXT_HOOK); } };
	  };
      }
      {
	SCM k, prompt;
	if ((_setjmp (((struct scm_prompt_registers*)((((scm_t_bits) (0? (*(SCM*)0=((((SCM *)((scm_t_cell *) (((scm_t_bits) (0? (*(SCM*)0=((((prompt))))): (((prompt)))))))) [((2))]))): (((SCM *)((scm_t_cell *) (((scm_t_bits) (0? (*(SCM*)0=((((prompt))))): (((prompt)))))))) [((2))]))))))->regs)))     {
	      { ip = vp->ip; sp = vp->sp; fp = vp->fp; };
	      { { if (__builtin_expect ((vp->trace_level > 0), 0)) { { vp->ip = ip; vp->sp = sp; vp->fp = fp; }; vm_dispatch_hook (vm, SCM_VM_NEXT_HOOK); } }; ; goto *jump_table[(*ip++) & ((1<<8)-1)]; };
	}

	if (__builtin_expect ((vp->trace_level > 0), 0)) { { vp->ip = ip; vp->sp = sp; vp->fp = fp; }; vm_dispatch_hook (vm, SCM_VM_NEXT_HOOK); } ;

      }
}
static const scm_t_vm_engine vm_engines[] =   {
    vm_regular_engine, vm_debug_engine };
SCM scm_c_vm_run (SCM vm, SCM program, SCM *argv, int nargs) {
    struct scm_vm *vp = ((struct scm_vm *) ((((scm_t_bits) (0? (*(SCM*)0=((((SCM *)((scm_t_cell *) (((scm_t_bits) (0? (*(SCM*)0=((((vm))))): (((vm)))))))) [((1))]))): (((SCM *)((scm_t_cell *) (((scm_t_bits) (0? (*(SCM*)0=((((vm))))): (((vm)))))))) [((1))]))))));
    return vm_engines[vp->engine](vm, program, argv, nargs);
}
