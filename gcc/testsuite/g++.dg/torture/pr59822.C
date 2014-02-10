// { dg-do compile }

typedef struct rtvec_def *rtvec;
enum machine_mode { VOIDmode };
struct rtvec_def { void *elem[1]; };
extern void *const_tiny_rtx[2];
void
ix86_build_const_vector (enum machine_mode mode, bool vect,
			 void *value, rtvec v, int n_elt)
{
  int i;
  for (i = 1; i < n_elt; ++i)
    ((v)->elem[i]) = vect ? value : (const_tiny_rtx[(int) (mode)]);
}
