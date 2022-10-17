/* { dg-additional-options "-O1" } */

void foo(void *);
struct chanset_t help_subst_chan;
struct chanset_t *help_subst_chan_0_0;
struct chanset_t {
  struct chanset_t *next;
  char dname[];
};
void help_subst(char *writeidx) {
  for (;; help_subst_chan = *help_subst_chan_0_0) {
    foo(help_subst_chan.next->dname);
    if (help_subst_chan_0_0) {
      writeidx++;
      *writeidx++ = ' ';
    }
  }
}
