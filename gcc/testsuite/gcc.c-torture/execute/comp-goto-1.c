#include <stdlib.h>

#if !defined(NO_LABEL_VALUES) && (!defined(STACK_SIZE) || STACK_SIZE >= 4000) && __INT_MAX__ >= 2147483647
typedef unsigned int uint32;
typedef signed int sint32;

typedef uint32 reg_t;

typedef unsigned long int host_addr_t;
typedef uint32 target_addr_t;
typedef sint32 target_saddr_t;

typedef union
{
  struct
    {
      unsigned int	offset:18;
      unsigned int	ignore:4;
      unsigned int	s1:8;
      int		:2;
      signed int	simm:14;
      unsigned int	s3:8;
      unsigned int	s2:8;
      int		pad2:2;
    } f1;
  long long ll;
  double d;
} insn_t;

typedef struct
{
  target_addr_t vaddr_tag;
  unsigned long int rigged_paddr;
} tlb_entry_t;

typedef struct
{
  insn_t *pc;
  reg_t registers[256];
  insn_t *program;
  tlb_entry_t tlb_tab[0x100];
} environment_t;

enum operations
{
  LOAD32_RR,
  METAOP_DONE
};

host_addr_t
f ()
{
  abort ();
}

reg_t
simulator_kernel (int what, environment_t *env)
{
  register insn_t *pc = env->pc;
  register reg_t *regs = env->registers;
  register insn_t insn;
  register int s1;
  register reg_t r2;
  register void *base_addr = &&sim_base_addr;
  register tlb_entry_t *tlb = env->tlb_tab;

  if (what != 0)
    {
      int i;
      static void *op_map[] =
	{
	  &&L_LOAD32_RR,
	  &&L_METAOP_DONE,
	};
      insn_t *program = env->program;
      for (i = 0; i < what; i++)
	program[i].f1.offset = op_map[program[i].f1.offset] - base_addr;
    }

 sim_base_addr:;

  insn = *pc++;
  r2 = (*(reg_t *) (((char *) regs) + (insn.f1.s2 << 2)));
  s1 = (insn.f1.s1 << 2);
  goto *(base_addr + insn.f1.offset);

 L_LOAD32_RR:
  {
    target_addr_t vaddr_page = r2 / 4096;
    unsigned int x = vaddr_page % 0x100;
    insn = *pc++;

    for (;;)
      {
	target_addr_t tag = tlb[x].vaddr_tag;
	host_addr_t rigged_paddr = tlb[x].rigged_paddr;

	if (tag == vaddr_page)
	  {
	    *(reg_t *) (((char *) regs) + s1) = *(uint32 *) (rigged_paddr + r2);
	    r2 = *(reg_t *) (((char *) regs) + (insn.f1.s2 << 2));
	    s1 = insn.f1.s1 << 2;
	    goto *(base_addr + insn.f1.offset);
	  }

	if (((target_saddr_t) tag < 0))
	  {
	    *(reg_t *) (((char *) regs) + s1) = *(uint32 *) f ();
	    r2 = *(reg_t *) (((char *) regs) + (insn.f1.s2 << 2));
	    s1 = insn.f1.s1 << 2;
	    goto *(base_addr + insn.f1.offset);
	  }

	x = (x - 1) % 0x100;
      }

    L_METAOP_DONE:
      return (*(reg_t *) (((char *) regs) + s1));
  }
}

insn_t program[2 + 1];

void *malloc ();

int
main ()
{
  environment_t env;
  insn_t insn;
  int i, res;
  host_addr_t a_page = (host_addr_t) malloc (2 * 4096);
  target_addr_t a_vaddr = 0x123450;
  target_addr_t vaddr_page = a_vaddr / 4096;
  a_page = (a_page + 4096 - 1) & -4096;

  env.tlb_tab[((vaddr_page) % 0x100)].vaddr_tag = vaddr_page;
  env.tlb_tab[((vaddr_page) % 0x100)].rigged_paddr = a_page - vaddr_page * 4096;
  insn.f1.offset = LOAD32_RR;
  env.registers[0] = 0;
  env.registers[2] = a_vaddr;
  *(sint32 *) (a_page + a_vaddr % 4096) = 88;
  insn.f1.s1 = 0;
  insn.f1.s2 = 2;

  for (i = 0; i < 2; i++)
    program[i] = insn;

  insn.f1.offset = METAOP_DONE;
  insn.f1.s1 = 0;
  program[2] = insn;

  env.pc = program;
  env.program = program;

  res = simulator_kernel (2 + 1, &env);

  if (res != 88)
    abort ();
  exit (0);
}
#else
main(){ exit (0); }
#endif
