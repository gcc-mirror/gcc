/* PR 65177 */
/* { dg-do compile } */
/* { dg-options "-O3" } */

typedef struct p7_profile_s {} P7_PROFILE;
enum p7t_statetype_e {
  p7T_S = 4,   p7T_N = 5,   p7T_E = 7,   p7T_C = 8,   p7T_J = 10, };
typedef struct p7_trace_s {} P7_TRACE;
typedef struct p7_gmx_s {
  int L;
} P7_GMX;
static inline int select_c(const P7_PROFILE *gm, const P7_GMX *pp, const P7_GMX *gx, int i) {
  float path[2];
  return ((path[0] > path[1]) ? p7T_C : p7T_E);
}
void p7_GOATrace(const P7_PROFILE *gm, const P7_GMX *pp, const P7_GMX *gx, P7_TRACE *tr) {
  int i = gx->L;
  int sprv, scur;
  while (sprv != p7T_S)     {
    switch (sprv) {       case p7T_C: scur = select_c(gm, pp, gx, i); break;       }
    if ( (scur == p7T_N || scur == p7T_J || scur == p7T_C) && scur == sprv) i--;
    sprv = scur;
  }
}
