extern int debug_threads;
extern void sigsuspend (void);
void my_waitpid (int flags, int wnohang)
{
  while (1)
    {
      if (flags & 0x80000000)
        {
          if (wnohang)
            break;
          if (debug_threads)
            __builtin_puts ("blocking\n");
          sigsuspend ();
        }
      flags ^= 0x80000000;
    }
}

