void prepare_to_wait (void *, void *, int);
void finish_wait (void *, void *);
extern signed long schedule_timeout (signed long);
struct sock
{
  unsigned char skc_state;
  void *sk_sleep;
  int sk_err;
};

void
sk_stream_wait_connect (struct sock *sk, long *timeo_p)
{
  int done;
  int wait;
  do
    {
      if ((1 << sk->skc_state) & ~12)
        return;
      prepare_to_wait (sk->sk_sleep, &wait, 1);
      *(timeo_p) = schedule_timeout (0);
      done = !sk->sk_err;
      finish_wait (sk->sk_sleep, &wait);
    }
  while (!done);
}
