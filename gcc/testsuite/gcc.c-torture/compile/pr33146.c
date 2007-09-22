typedef struct
{
  int end;
  int term;
}
jpc_enc_pass_t;
void foo(int numpasses, jpc_enc_pass_t *p)
{
  jpc_enc_pass_t *pass;
  jpc_enc_pass_t *termpass;
  for (pass = p; pass != termpass; ++pass)
    if (!pass->term)
    {
      termpass = pass;
      while (termpass - pass < numpasses && !termpass->term)
        ++termpass;
      pass->end = termpass->end;
    }
}
