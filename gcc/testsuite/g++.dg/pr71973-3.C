// { dg-do compile }
// { dg-options "-Wall -fdump-tree-eh" }

extern "C"
int execve (const char *__path, char *const __argv[], char *const __envp[])
__attribute__ ((__nothrow__));

void foo () throw ()
{
  execve (0,0,0);
}

// { dg-final { scan-tree-dump-not "eh_dispatch" "eh" } }
// { dg-final { scan-tree-dump-not "resx" "eh" } }
