/* { dg-additional-options "-O2" } */

extern void free (void *__ptr) __attribute__ ((__nothrow__ , __leaf__));

enum pipecmd_tag
{
 PIPECMD_PROCESS,
 PIPECMD_SEQUENCE
};

struct pipecmd {
 enum pipecmd_tag tag;
 union {
  struct pipecmd_process {
   int argc;
   int argv_max;
   char **argv;
  } process;
  struct pipecmd_sequence {
   int ncommands;
   int commands_max;
   struct pipecmd **commands;
  } sequence;
 } u;
};

static char *argstr_get_word (const char **argstr)
{
 while (**argstr) {
  switch (**argstr) {
   case ' ':
   case '\t':
    return (void *) 0;
  }
 }
 return (void *) 0;
}

struct pipecmd *pipecmd_new_argstr (const char *argstr)
{
 argstr_get_word (&argstr);
 return (void *) 0;
}

void pipecmd_free (struct pipecmd *cmd)
{
 int i;

 if (!cmd)
  return;

 switch (cmd->tag) {
  case PIPECMD_PROCESS: {
   struct pipecmd_process *cmdp = &cmd->u.process;

   for (i = 0; i < cmdp->argc; ++i)
    free (cmdp->argv[i]);
   free (cmdp->argv);

   break;
  }

  case PIPECMD_SEQUENCE: {
   struct pipecmd_sequence *cmds = &cmd->u.sequence;

   for (i = 0; i < cmds->ncommands; ++i)
    pipecmd_free (cmds->commands[i]);
   free (cmds->commands);

   break;
  }
 }

 free (cmd);
}
