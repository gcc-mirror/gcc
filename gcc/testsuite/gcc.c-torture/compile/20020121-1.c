/* This testcase resulted in a 'unrecognizeable insn' on powerpc-linux-gnu
   because of a missing trunc_int_for_mode in simplify_and_const_int.  */

struct display {
  struct disphist *hstent;
  int pid;
  int status;
};

struct disphist {
  struct disphist *next;
  char *name;
  int startTries;
  unsigned rLogin:2,
    sd_how:2,
    sd_when:2,
    lock:1,
    goodExit:1;
  char *nuser, *npass, **nargs;
};

void
StartDisplay (struct display *d)
{
  d->pid = 0;
  d->status = 0;
  d->hstent->lock = d->hstent->rLogin = d->hstent->goodExit =
    d->hstent->sd_how = d->hstent->sd_when = 0;
}

