// Build don't link:

typedef void (*__sighandler_t)(int);

struct sigaction {
  __sighandler_t sa_handler;
};

struct task_struct {
  struct sigaction sigaction[32];
};

void
get_stat() {
  struct task_struct ** p = 0;
  unsigned long bit = 1;
  unsigned long sigignore = 0;
  int i = 0;
  switch((__SIZE_TYPE__) (*p)->sigaction[i].sa_handler)
    {
    case 1:
      sigignore |= bit;
      break;
    }
}
