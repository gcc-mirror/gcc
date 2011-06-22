/* { dg-do run } */
/* { dg-require-effective-target tls_runtime } */
/* { dg-add-options tls } */
/* { dg-skip-if "TLS local exec code cannot be linked into shared objects" { alpha*-*-linux* } { "-pie" } { "" } } */

extern void abort (void);

__thread int tls_le __attribute__((tls_model("local-exec"))) = 3;

int get_le (void)
{
  return tls_le;
}

int *get_lep (void)
{
  return &tls_le;
}

int main (void)
{
  int val;

  val = get_le ();
  if (val != 3)
    abort ();

  val = *get_lep ();
  if (val != 3)
    abort ();

  return 0;
}
