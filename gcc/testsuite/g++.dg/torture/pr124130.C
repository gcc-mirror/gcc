// { dg-do compile }

void __sigsetjmp_cancel() __attribute__((__returns_twice__));
struct basic_ostream {
  basic_ostream &operator<<(basic_ostream &__pf(basic_ostream &)) {
    return __pf(*this);
  }
} cerr;
extern "C" void _exit(int);
enum { Exit_Internal_Error };
basic_ostream &report_error(basic_ostream &) { _exit(Exit_Internal_Error); }
void thread_pool_thread_main() {
  __sigsetjmp_cancel();
  cerr << report_error;
}
