#include <arm_cmse.h>

union u_t_0 {
  long long ll;
};

typedef void(__attribute__((cmse_nonsecure_call)) fn_t_0)(union u_t_0);

void fn_caller_0(fn_t_0 *f_ptr) {
  union u_t_0 x = {1234};
  f_ptr(x);
}

union u_t_1 {
  long int l;
};

typedef void(__attribute__((cmse_nonsecure_call)) fn_t_1)(union u_t_1);

void fn_caller_1(fn_t_1 *f_ptr) {
  union u_t_1 x = {1234};
  f_ptr(x);
}
