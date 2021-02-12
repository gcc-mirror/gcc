/* PR target/99041 */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -w" } */

/* Verify we do not ICE on the following source.  */

long a, b, c, d, e;
 enum { f };
 enum { g, aa };
 enum { h };
 template < typename > struct ai;
 template < typename > struct ad;
 template < typename, int, int ah, int = 0, int = f, int = ah > class ab;
 template < typename > class ar;
 template < typename > class ak;
 template < typename, int, int = 1, bool = false > class aj;
 template < typename > class an;
 template < typename, typename > class al;
 template < typename, typename, int = h > class am;
 template < typename, unsigned > class ao;
 template < typename > struct ap;
 template < typename aq > struct av { typedef ar< aq > ac; };
 template < typename > struct as {   typedef av< am< al< int, an< aj< ab< double, 5, 2 >, false > > >, int > >::ac       ac; };
 template < typename at > at au(const typename ap< at >::ac *);
 template < typename at, int > at cc(typename ap< at >::ac *aw) {   return au< at >(aw); }
 typedef __attribute__((altivec(vector__))) double ax;
 template <> struct ap< ax > { typedef double ac; };
 template <> ax au(const double *aw) { return __builtin_vec_vsx_ld(0, aw); }
 template < typename > struct ay {};
 template < typename aq > class ae : public ad< aq > { public:   typedef typename ai< aq >::ba ba; };
 template < typename aq > class ar : public ae< aq > { public:   ak< aq > bk(); };
 template < typename > struct ad {};
 template < int > class bc;
 template < typename bd, typename bf, int bg > struct ai< am< bd, bf, bg > > {   typedef typename bd::ba ba; };
 template < typename bh, typename bj, int bg > class am : public bc< bg > { public:   bh bu();   bj k(); };
 template < int bg > class bc : public as< am< int, int, bg > >::ac {};
 template < typename, typename, typename > struct l;
 template < typename m, typename bl, typename ag > void n(m bm, bl bn, ag bo) {   l< m, bl, ag >::bz(bm, bn, bo); }
 template < typename, typename, typename, typename > struct bp;
 class o { public:   o(double *, long);   double &operator()(long i, long j) { return p[aa ? j + i * w : w]; }   template < typename q, int t > q br(long i, long j) {     double &cl = operator()(i, j);     return cc< q, t >(&cl);   }   double *p;   long w; };
 class bt : public o { public:   bt(double *cf, long bv) : o(cf, bv) {} };
 struct bw {   enum { bx }; };
 template < typename bq > class ak { public:   template < typename by > void operator=(by) {     am< al< const an< const aj< aj< ab< double, 5, 5, 2 >, -1, -1 >, -1 > >,             const an< const aj< aj< ab< double, 5, 5, 2 >, -1, -1 >, -1 > > >,         ao< aj< ab< double, 5, 0 >, 1 >, 0 > >         ck;     n(ca, ck, ay< typename by::ba >());   }   bq ca; };
 template < typename aq > class dn : public av< aq >::ac {};
 template < typename cd, int af, int ah, int ce, int co, int cg > struct ai< ab< cd, af, ah, ce, co, cg > > {   typedef cd ba;   enum { bs }; };
 template < typename, int, int, int, int co, int cg > class ab : public dn< ab< int, co, cg > > { public:   template < typename by > ab(by); };
 template < typename, typename ch > class al : public ch {};
 template < typename aq > class az { public:   typedef typename ai< aq >::ba ba;   typedef const ba *ci;   ba &cj(long); };
 template < typename ct > class bb : public az< ct > { public:   bb(typename bb::ci, long); };
 template < typename cx, int cm, int cn, bool da > struct ai< aj< cx, cm, cn, da > > : ai< cx > {};
 template < typename > class cp;
 template < typename cx, int, int, bool > class aj : public cp< cx > { public:   aj(cx, long, long, long, long); };
 template < typename cx > class cp : public av< aj< cx, 1 > >::ac {};
 template < typename cq > class an : public cq {};
 template < typename cq, unsigned cr > struct ai< ao< cq, cr > > {   typedef cq cs; };
 template < typename, unsigned > class ao { public:   typedef aj< ab< double, 5, 0 >, 1 > cq;   typename ai< ao >::cs cu(); };
 template < typename, typename > struct cv;
 template < typename cw, typename bd, typename bf, int dh, typename ba > struct l< cw, am< bd, bf, dh >, ay< ba > > {   static void bz(cw bm, am< bd, bf > bn, ay< ba >) {     cv< bd, bf >::cy(bm, bn.bu(), bn.k());   } };
 template < typename bf, typename aq > struct cz {   template < typename m >   static void   cy(m bm,      al< const an< const aj< aj< ab< double, 5, 5, 2 >, -1, -1 >, -1 > >,          const an< const aj< aj< ab< double, 5, 5, 2 >, -1, -1 >, -1 > > >          bu,      bf) {     dl(bm, bu);   }   template < typename m >   static void   dl(m bm, al< const an< const aj< aj< ab< double, 5, 5, 2 >, -1, -1 >, -1 > >,                const an< const aj< aj< ab< double, 5, 5, 2 >, -1, -1 >, -1 > > >                bu) {     typename alpha;     bf k;     aq::dl(bm, bu, k, alpha);   } };
 template < typename, typename, bool > struct u;
 template < typename bd, typename bf > struct cv : cz< bf, cv< bd, bf > > {   template < typename v >   static void dl(v bm, bd bu, bf k, typename am< bd, bf >::ba alpha) {     u< bd, typename bf::cq, false >::bz(bm, bu, k.cu(), alpha);   } };
 template < typename, int, typename, bool, typename, bool, int, int = g > struct db;
 template < typename cb, int x, typename z, bool dc, typename dd, bool de,            int df > struct db< cb, x, z, dc, dd, de, aa, df > {   typedef z dg;   static void bz(cb, cb, const z *, cb, const dd *, cb, dg *, cb, const dg &); };
 template < typename cb, int x, typename z, bool dc, typename dd, bool de,            int df > void db< cb, x, z, dc, dd, de, aa, df >::bz(cb, cb, const z *_lhs, cb di,                                             const dd *dj, cb dk, dg *_res,                                             cb dm, const dg &alpha) {   cb cols;   bb< ab< z, 1, aa > > bu(_lhs, cols), res(_res, dm);   bb< ab< dd, 1, 1 > > k(dj, cols);   for (cb pi;;) {     cb actualPanelWidth, s, r;     bp< cb, bt, dd, bt >::bz(actualPanelWidth, r, bt(&bu.cj(s), di),                              bt(&k.cj(s), dk), &res.cj(pi), dm, alpha);   } }
 template < int, int > struct trmv_selector;
 template < typename bd, typename bf > struct u< bd, bf, false > {   template < typename v >   static void bz(v bm, bd bu, bf k, typename v::ba alpha) {     v dstT(bm);     trmv_selector< 0, ai< bf >::bs ?: aa >::bz(k, bu, dstT, alpha);   } };
 template < int x > struct trmv_selector< x, aa > {   template < typename bd, typename bf, typename v >   static void bz(bd, bf, v, typename v::ba) {     typedef typename bf::ba dd;     typedef bw LhsBlasTraits;     typedef bw RhsBlasTraits;     typename actualAlpha;     dd actualRhsPtr;     db< long, x, typename bd::ba, LhsBlasTraits::bx, dd, RhsBlasTraits::bx,         aa >::bz(0, 0, 0, 0, &actualRhsPtr, 1, 0, 0, actualAlpha);   } };
 template < typename LhsPacket, typename, bool > void pger_vec(__vector_quad *, __vector_pair &, LhsPacket);
 template < typename, typename, typename, typename, typename, typename > void gemv_row(bt alhs) {   typedef ax LhsPacket;   typedef ax RhsPacket;   bt bu(alhs);   enum { LhsAlignment, LhsPacketSize };   long i, j;   __vector_quad c0;   for (;; j += LhsPacketSize) {     RhsPacket b0;     __vector_pair b00;     __builtin_mma_assemble_pair(&b00,                                 (__attribute__((altivec(vector__))) char)                                     bu.br< LhsPacket, LhsAlignment >(1, j),                                 (__attribute__((altivec(vector__))) char)                                     bu.br< LhsPacket, LhsAlignment >(i, j));     pger_vec< LhsPacket, RhsPacket, true >(&c0, b00, b0);   } }
 template < typename cb, typename LhsMapper, typename RhsMapper > struct bp< cb, LhsMapper, double, RhsMapper > {   static void bz(cb, cb, LhsMapper bu, RhsMapper, double *, cb, double) {     gemv_row< cb, double, LhsMapper, double, RhsMapper, double >(bu);   } };
 class be { public:   template < typename v, typename Workspace > void cy(v, Workspace workspace) {     applyThisOnTheLeft(workspace, true);   }   template < typename v, typename Workspace >   void applyThisOnTheLeft(v bm, Workspace) {     aj< ab< double, 5, 0 >, 1 > sub_vecs1(m_vectors, c, a, c, b);     aj< v, 1 > sub_dst(bm, d, 0, e, 0);     apply_block_householder_on_the_left(sub_dst, sub_vecs1, b);   }   ab< double, 5, 5 > m_vectors; };
 template < typename TriangularFactorType, typename VectorsType,            typename CoeffsType > void make_block_householder_triangular_factor(TriangularFactorType triFactor,                                               VectorsType vectors, CoeffsType) {   triFactor.bk() = vectors; }
 template < typename cq, typename VectorsType, typename CoeffsType > void apply_block_householder_on_the_left(cq, VectorsType vectors,                                          CoeffsType hCoeffs) {   enum { TFactorSize };   ab< typename cq::ba, TFactorSize, aa > y(0);   make_block_householder_triangular_factor(y, vectors, hCoeffs); }
 class HessenbergDecomposition { public:   be matrixQ(); };
 template < typename > class RealSchur { public:   enum { MaxColsAtCompileTime };   template < typename InputType >   RealSchur &compute(const ad< InputType > &, bool);   ab< double, 5, 5 > m_matU;   ab< double, MaxColsAtCompileTime, 1 > m_workspaceVector;   HessenbergDecomposition m_hess; };
 template < typename cq > template < typename InputType > RealSchur< cq > &RealSchur< cq >::compute(const ad< InputType > &, bool) {   m_hess.matrixQ().cy(m_matU, m_workspaceVector); }
 template < typename > class EigenSolver { public:   EigenSolver();   template < typename InputType >   EigenSolver &compute(const ad< InputType > &, bool = true);   RealSchur< int > m_realSchur; };
 template < typename cq > template < typename InputType > EigenSolver< cq > &EigenSolver< cq >::compute(const ad< InputType > &matrix,                                               bool computeEigenvectors) {   m_realSchur.compute(matrix, computeEigenvectors); }
 class PolynomialSolver { public:   PolynomialSolver(ab< double, 0, 1 >) {     ab< int, 0, 0 > __trans_tmp_1 = m_eigenSolver.compute(__trans_tmp_1);   }   EigenSolver< double > m_eigenSolver; };
 struct increment_if_fixed_size {   enum { bi }; };
 template < int, typename, typename > void aux_evalSolver(ab< double, 0, 1 > pols) {   PolynomialSolver solve_constr(pols); }
 template < int Deg, typename > void evalSolver(ab< double, 0, 1 > pols) {   aux_evalSolver< Deg, ab< double, 0, 1 >, PolynomialSolver >(pols); }
 template < typename, int _Deg > void polynomialsolver(int) {   ab< double, increment_if_fixed_size::bi, 1 > pols = 1;   evalSolver< _Deg, ab< double, increment_if_fixed_size::bi, 1 > >(pols); }
 void test_polynomialsolver() { polynomialsolver< double, 5 >(5); }
