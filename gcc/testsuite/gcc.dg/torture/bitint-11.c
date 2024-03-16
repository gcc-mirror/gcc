/* PR c/102989 */
/* { dg-do run { target { bitint && int32plus } } } */
/* { dg-options "-std=gnu23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#ifdef __SIZEOF_INT128__
#define int128_val(x, y) x
#define int128_bitfld(x) x
#define int128 __int128
#else
#define int128_val(x, y) y
#define int128_bitfld(x) 1
#define int128 int
#endif

struct S {
  int i : 1;
  int j : 27;
  long long k : 56;
  int128 l : int128_bitfld (120);
  unsigned m : 3;
  unsigned long long n : 42;
  unsigned int128 o : int128_bitfld (125);
};

#if __BITINT_MAXWIDTH__ >= 188
__attribute__((noipa)) _BitInt(188)
test188 (int a, long long b, int128 c, unsigned short d,
	 unsigned long long e, unsigned int128 f, _BitInt(188) *g,
	 struct S *p)
{
  return *g + a + b + c + d + e + f + p->i + p->j + p->k + p->l + p->m + p->n + p->o;
}
#endif

#if __BITINT_MAXWIDTH__ >= 575
__attribute__((noipa)) _BitInt(575)
test575 (int a, long long b, int128 c, unsigned short d,
	 unsigned long long e, unsigned int128 f, _BitInt(575) *g,
	 struct S *p)
{
  return *g + a + b + c + d + e + f + p->i + p->j + p->k + p->l + p->m + p->n + p->o;
}
#endif

int
main ()
{
  struct S s = { -1, 34877496, -5580713461260169LL,
		 int128_val (329913978417840804084945075673166782wb, 0),
		 1U, 68262565354ULL,
		 int128_val (5169264897230903304574466460949339423uwb, 0) };
  (void) &s;
#if __BITINT_MAXWIDTH__ >= 188
  _BitInt(188) b188 = 51625354135609169507415940782602403562057573230992065937wb;
  if (test188 (-943502954, 6037882847713389759LL,
	       int128_val (-83021930809076074533830959108195408798wb, 0),
	       55564, 16536837072610496559ULL,
	       int128_val (237527410537432634851289433535592281366uwb, 0),
	       &b188, &s)
      != int128_val (51625354135609169667420599386607708010744598469228066319wb,
		     51625354135609169507415940782602403584626712505208687546wb))
   __builtin_abort ();
#endif
#if __BITINT_MAXWIDTH__ >= 575
  _BitInt(575) b575 = -20179593413257295974034559327597105210986698104682733481090257927308152867107766179792744096926808179875378122328280563720406829353405990702024578604764643194143342705864832wb;
  if (test575 (-943502954, 6037882847713389759LL,
	       int128_val (-83021930809076074533830959108195408798wb, 0),
	       55564, 16536837072610496559ULL,
	       int128_val (237527410537432634851289433535592281366uwb, 0),
	       &b575, &s)
      != int128_val (-20179593413257295974034559327597105210986698104682733481090257927308152867107766179792744096926808179875378122328280563720406829353405830697365974599460194507118104469864450wb,
		     -20179593413257295974034559327597105210986698104682733481090257927308152867107766179792744096926808179875378122328280563720406829353405990702024578604764620625004068489243223wb))
   __builtin_abort ();
#endif
}
