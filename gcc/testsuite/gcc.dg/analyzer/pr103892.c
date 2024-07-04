/* { dg-additional-options "-O2 -Wno-analyzer-too-complex -Wno-analyzer-symbol-too-complex" } */

/* C only: C++ FE optimizes argstr_get_word completely away
   and therefore the number of SN diminishes compared to C,
   making the analysis bails out early.  */

extern void free (void *__ptr) __attribute__ ((__nothrow__ , __leaf__));

enum pipecmd_tag
{
 PIPECMD_PROCESS,
 PIPECMD_SEQUENCE
};

struct pipecmd {
 enum pipecmd_tag tag;
 union pipecmd_union_t {
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

#ifdef __cplusplus
typedef pipecmd::pipecmd_union_t::pipecmd_process pipecmd_process_t;
typedef pipecmd::pipecmd_union_t::pipecmd_sequence pipecmd_sequence_t;
#else
typedef struct pipecmd_process pipecmd_process_t;
typedef struct pipecmd_sequence pipecmd_sequence_t;
#endif

static char *argstr_get_word (const char **argstr)
{
 while (**argstr) { /* { dg-warning "infinite loop" } */
  switch (**argstr) {
   case ' ':
   case '\t':
    return (char *) ((void *) 0);
  }
 }
 return (char *) ((void *) 0);
}

struct pipecmd *pipecmd_new_argstr (const char *argstr)
{
 argstr_get_word (&argstr);
 return (struct pipecmd *) ((void *) 0);
}

void pipecmd_free (struct pipecmd *cmd)
{
 int i;

 if (!cmd)
  return;

 switch (cmd->tag) {
  case PIPECMD_PROCESS: {
   pipecmd_process_t *cmdp = &cmd->u.process;

   for (i = 0; i < cmdp->argc; ++i)
    free (cmdp->argv[i]);
   free (cmdp->argv);

   break;
  }

  case PIPECMD_SEQUENCE: {
   pipecmd_sequence_t *cmds = &cmd->u.sequence;

   for (i = 0; i < cmds->ncommands; ++i)
    pipecmd_free (cmds->commands[i]);
   free (cmds->commands);

   break;
  }
 }

 free (cmd);
}
