// { dg-do run { target { ! ia32 } } }
// { dg-require-effective-target fstack_protector }
// { dg-options "-O2 -fno-strict-aliasing -msse4.2 -mfpmath=sse -fPIC -fstack-protector-strong -O2" }

struct p2_icode *ipc;
register int pars asm("r13");
register struct processor *cur_pro asm("rbp");
register int a asm("rbx");
register int c asm("r14");
typedef long lina_t;
typedef long la_t;
typedef processor processor_t;
typedef p2_icode p2_icode_t;
typedef enum {
  P2_Return_Action_Next,
} p2_return_action_t;
typedef struct p2_icode {
  int ic_Parameters;
}  icode_t;
extern "C" icode_t *x86_log_to_icode_exec(processor_t *, la_t);
typedef struct {
  icode_t *ipc;
} b;
typedef struct {
  char ma_thread_signal;
  int event_counter;
  b instrumentation;
} d;

extern "C" lina_t int2linaddr(processor_t *cpu, const p2_icode_t *ic)
{
  return 0;
}

typedef struct e {
  long i64;
  char LMA;
} f;

struct processor {
  d common;
  e pc_RIP;
  f pc_EFER;
  p2_icode_t *saved_ipc;
};
inline la_t code_lin_to_log(processor_t *, long) { return 0; }
void turbo_clear(processor_t *) {}

p2_return_action_t p2_ep_REBIND_IPC(void)
{
  processor_t *cpu = cur_pro;
  la_t vaddr = cpu->pc_RIP.i64;
  cur_pro->saved_ipc = (p2_icode_t *) ipc;
  cur_pro->common.instrumentation.ipc = ipc;
  cur_pro->pc_RIP.i64 = code_lin_to_log(cur_pro, int2linaddr(cur_pro, ipc));
  turbo_clear(cur_pro);

  cpu->saved_ipc = x86_log_to_icode_exec(cur_pro, vaddr);
  ipc++;
  (cur_pro->common.event_counter -= (1));
  if (__builtin_expect((!((cur_pro->common.event_counter <= 0)
			  | cur_pro->common.ma_thread_signal)), 1))
    {
      ((pars = ((ipc)->ic_Parameters)));
      return P2_Return_Action_Next;
    } else {
      return (p2_return_action_t) 0;
    }
  return P2_Return_Action_Next;
}

struct p2_icode fake_ipc = { 0 };
struct processor fake_proc ={{ 0 } };

extern "C" icode_t *
x86_log_to_icode_exec(processor_t *cpu, la_t la)
{
  return 0;
}

extern "C" void
turbo_threshold_reached(processor_t *c, p2_icode_t *i, int s)
{
}

int main()
{
  if (!__builtin_cpu_supports ("sse4.2"))
    return 0;
  fake_proc.pc_RIP.i64 = 0xbaadc0de;
  fake_proc.pc_EFER.LMA = 0xf;
  ipc = &fake_ipc;
  cur_pro = &fake_proc;
  p2_ep_REBIND_IPC();
  return 0;
}
