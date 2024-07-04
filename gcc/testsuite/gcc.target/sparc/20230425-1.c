/* PR target/105573 */
/* Reported by Sam James <sjames@gcc.gnu.org> */

/* { dg-do compile } */
/* { dg-options "-O3 -mvis3" } */

int *UINT_sign_args, UINT_sign_steps;
int *UINT_sign_ip1;

void UINT_sign() {
  char *op1 = (char*) UINT_sign_args;
  int os1 = UINT_sign_steps, i;
  for (; i; i++, op1 += os1) {
    unsigned in = *(unsigned *)UINT_sign_ip1;
    int *out = (int*) op1;
    *out = in > 0;
  }
}
