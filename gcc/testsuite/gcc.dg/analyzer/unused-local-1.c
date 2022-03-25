/* { dg-additional-options "-fdump-analyzer-untracked" } */

struct st
{
  const char *m_filename;
  int m_line;
  const char *m_function;
};

extern void debug (struct st *);

void test (void)
{
  {
    static struct st s1 = { __FILE__, __LINE__, __func__ }; /* { dg-warning "track 's1': no" } */
    debug (&s1);
  }
  {
    static struct st s2 = { __FILE__, __LINE__, __func__ }; /* { dg-warning "track 's2': no" } */
    debug (&s2);
  }
}
