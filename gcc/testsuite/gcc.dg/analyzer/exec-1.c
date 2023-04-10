#define NULL ((void *)0)

extern int execl(const char *pathname, const char *arg, ...);
extern int execlp(const char *file, const char *arg, ...);
extern int execle(const char *pathname, const char *arg, ...);
extern int execv(const char *pathname, char *const argv[]);
extern int execvp(const char *file, char *const argv[]);
extern int execvpe(const char *file, char *const argv[], char *const envp[]);

int test_execl_ls_al ()
{
  return execl ("/usr/bin/ls", "ls", "-al", NULL);
}

int test_execlpl_ls_al ()
{
  return execlp ("ls", "ls", "-al", NULL);
}

int test_execle_ls_al ()
{
  const char *env[3] = {"FOO=BAR", "BAZ", NULL};
  return execl ("/usr/bin/ls", "ls", "-al", NULL, env);
}

int test_execv_ls_al ()
{
  char *argv[3] = {"ls", "-al", NULL};
  return execv ("/usr/bin/ls", argv);
}

int test_execvp_ls_al ()
{
  char *argv[3] = {"ls", "-al", NULL};
  return execvp ("ls", argv);
}

int test_execvpe_ls_al ()
{
  char *env[3] = {"FOO=BAR", "BAZ", NULL};
  char *argv[3] = {"ls", "-al", NULL};
  return execvpe ("ls", argv, env);
}
