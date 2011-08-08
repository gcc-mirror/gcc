/* { dg-do compile } */
/* { dg-options "-O0" } */
/* { dg-require-effective-target tls } */

struct gomp_team_state
{
  struct gomp_team_state *prev_ts;
  unsigned team_id;
  unsigned level;
};
struct gomp_thread
{
  void *data;
  struct gomp_team_state ts;
};
extern __thread struct gomp_thread gomp_tls_data;
int
foo (int level)
{
  struct gomp_team_state *ts = &gomp_tls_data.ts;
  if (level < 0 || level > ts->level)
    return -1;
  return ts->team_id;
}
