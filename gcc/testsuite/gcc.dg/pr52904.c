/* { dg-do compile } */
/* { dg-options "-Wstrict-overflow -O2" } */
extern int foo (int);

int
wait_reading_process_output (void)
{
  int nfds = 0;
  int channel;

  for (channel = 0; channel < 1024; ++channel)
    {
      if (foo (channel))
	nfds++;
    }

  if (nfds < 0) /* { dg-bogus "assuming signed overflow does not occur" "" } */
    return 1;
  return 0;
}
