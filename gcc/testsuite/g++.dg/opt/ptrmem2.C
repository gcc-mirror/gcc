typedef unsigned int Mword;
struct Thread
{
  Mword sys_ipc_log();
  void hook_ipc_vector();
  unsigned (Thread::*syscall_table)();
};

void Thread::hook_ipc_vector()
{
  syscall_table = &Thread::sys_ipc_log;
}
