/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

struct machine_thread_all_state {
  int set;
} _hurd_setup_sighandler_state;
int _hurd_setup_sighandler_ss_0;
struct {
  int ctx;
} *_hurd_setup_sighandler_stackframe;
void _setjmp();
void __thread_get_state();
int machine_get_basic_state(struct machine_thread_all_state *state) {
  if (state->set)
    __thread_get_state();
  return 1;
}
int *_hurd_setup_sighandler() {
  int *scp;				/* { dg-bogus "used uninitialized" } */
  if (_hurd_setup_sighandler_ss_0) {
    _setjmp();
    _hurd_setup_sighandler_state.set |= 5;
  }
  machine_get_basic_state(&_hurd_setup_sighandler_state);
  scp = &_hurd_setup_sighandler_stackframe->ctx;
  _setjmp();
  return scp;
}
