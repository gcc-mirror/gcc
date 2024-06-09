/* PR c/102989 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#include <stdatomic.h>

extern void abort (void);

#if __BITINT_MAXWIDTH__ >= 127
_Atomic _BitInt(127) v;
_BitInt(127) count, res;
const _BitInt(127) init = ~(_BitInt(127)) 0wb;

void
test_fetch_add ()
{
  atomic_init (&v, 13505789527944801758751150119415226784wb);
  count = -64910836855286429164283779649638556795wb;

  if (atomic_fetch_add_explicit (&v, count, memory_order_relaxed)
      != 13505789527944801758751150119415226784wb)
    abort ();

  if (atomic_fetch_add_explicit (&v, 2227507280963412295355244564739509222wb,
				 memory_order_consume)
      != -51405047327341627405532629530223330011wb)
    abort ();

  if (atomic_fetch_add_explicit (&v, count, memory_order_acquire)
      != -49177540046378215110177384965483820789wb)
    abort ();

  if (atomic_fetch_add_explicit (&v, 42245667388877614520169143618236120405wb,
				 memory_order_release)
      != 56052806558804587457226139100761728144wb)
    abort ();

  if (atomic_fetch_add_explicit (&v, count, memory_order_acq_rel)
      != -71842709512787029754292020996886257179wb)
    abort ();

  if (atomic_fetch_add_explicit (&v, 77995075987640754057679086146674392947wb,
				 memory_order_seq_cst)
      != 33387637092395772813111503069359291754wb)
    abort ();

  if (atomic_fetch_add (&v, 11810767284628435493328779846084830297wb)
      != -58758470380432704860896714499850421027wb)
    abort ();

  if (atomic_load (&v) != -46947703095804269367567934653765590730wb)
    abort ();
}

void
test_fetch_sub ()
{
  atomic_store_explicit (&v, 30796781768365552851024605388374299173wb,
			 memory_order_release);
  count = 32457177597484647488149720668185011722wb;

  if (atomic_fetch_sub_explicit (&v, count, memory_order_relaxed)
      != 30796781768365552851024605388374299173wb)
    abort ();

  if (atomic_fetch_sub_explicit (&v, 54614103079293459991417218347656369566wb,
				 memory_order_consume)
      != -1660395829119094637125115279810712549wb)
    abort ();

  if (atomic_fetch_sub_explicit (&v, count, memory_order_acquire)
      != -56274498908412554628542333627467082115wb)
    abort ();

  if (atomic_fetch_sub_explicit (&v, -44514083923735151931107302009741400482wb,
				 memory_order_release)
      != 81409506954572029614995249420232011891wb)
    abort ();

  if (atomic_fetch_sub_explicit (&v, count, memory_order_acq_rel)
      != -44217592582162050185584752285910693355wb)
    abort ();

  if (atomic_fetch_sub_explicit (&v, 30348078982452392099140613411731040827wb,
				 memory_order_seq_cst)
      != -76674770179646697673734472954095705077wb)
    abort ();

  if (atomic_fetch_sub (&v, -82224045897086857020012824788652775087wb)
      != 63118334298370141958812217350057359824wb)
    abort ();

  if (atomic_load_explicit (&v, memory_order_acquire)
      != -24798803265012232752862261577173970817wb)
    abort ();
}

void
test_fetch_and ()
{
  atomic_store (&v, init);

  if (atomic_fetch_and_explicit (&v, 0, memory_order_relaxed) != init)
    abort ();

  if (atomic_fetch_and_explicit (&v, init, memory_order_consume) != 0)
    abort ();

  if (atomic_fetch_and_explicit (&v, 0, memory_order_acquire) != 0)
    abort ();

  v = ~v;
  if (atomic_fetch_and_explicit (&v, init, memory_order_release) != init)
    abort ();

  if (atomic_fetch_and_explicit (&v, 0, memory_order_acq_rel) != init)
    abort ();

  if (atomic_fetch_and_explicit (&v, 0, memory_order_seq_cst) != 0)
    abort ();

  if (atomic_fetch_and (&v, 0) != 0)
    abort ();
}

void
test_fetch_xor ()
{
  v = init;
  count = 0;

  if (atomic_fetch_xor_explicit (&v, count, memory_order_relaxed) != init)
    abort ();

  if (atomic_fetch_xor_explicit (&v, ~count, memory_order_consume) != init)
    abort ();

  if (atomic_fetch_xor_explicit (&v, 0, memory_order_acquire) != 0)
    abort ();

  if (atomic_fetch_xor_explicit (&v, ~count, memory_order_release) != 0)
    abort ();

  if (atomic_fetch_xor_explicit (&v, 0, memory_order_acq_rel) != init)
    abort ();

  if (atomic_fetch_xor_explicit (&v, ~count, memory_order_seq_cst) != init)
    abort ();

  if (atomic_fetch_xor (&v, ~count) != 0)
    abort ();
}

void
test_fetch_or ()
{
  v = 0;
  count = 17592186044416wb;

  if (atomic_fetch_or_explicit (&v, count, memory_order_relaxed) != 0)
    abort ();

  count *= 2;
  if (atomic_fetch_or_explicit (&v, 35184372088832wb, memory_order_consume)
      != 17592186044416wb)
    abort ();

  count *= 2;
  if (atomic_fetch_or_explicit (&v, count, memory_order_acquire)
      != 52776558133248wb)
    abort ();

  count *= 2;
  if (atomic_fetch_or_explicit (&v, 140737488355328wb, memory_order_release)
      != 123145302310912wb)
    abort ();

  count *= 2;
  if (atomic_fetch_or_explicit (&v, count, memory_order_acq_rel)
      != 263882790666240wb)
    abort ();

  count *= 2;
  if (atomic_fetch_or_explicit (&v, count, memory_order_seq_cst)
      != 545357767376896wb)
    abort ();

  count *= 2;
  if (atomic_fetch_or (&v, count) != 1108307720798208wb)
    abort ();
}


/* Test the OP routines with a result which isn't used.  */

void
test_add ()
{
  v = 0;
  count = 4722366482869645213696wb;

  atomic_fetch_add (&v, count);
  if (v != 4722366482869645213696wb)
    abort ();

  atomic_fetch_add_explicit (&v, count, memory_order_consume);
  if (v != 9444732965739290427392wb)
    abort ();

  atomic_fetch_add (&v, 4722366482869645213696wb);
  if (v != 14167099448608935641088wb)
    abort ();

  atomic_fetch_add_explicit (&v, 4722366482869645213696wb,
			     memory_order_release);
  if (v != 18889465931478580854784wb)
    abort ();

  atomic_fetch_add (&v, 4722366482869645213696wb);
  if (v != 23611832414348226068480wb)
    abort ();

  atomic_fetch_add_explicit (&v, count, memory_order_seq_cst);
  if (v != 28334198897217871282176wb)
    abort ();
}

void
test_sub ()
{
  v = res = -3638804536836293398783417724445294828wb;
  count = 0;

  atomic_fetch_sub (&v, count + 1);
  if (v != --res)
    abort ();

  atomic_fetch_sub_explicit (&v, count + 1, memory_order_consume);
  if (v != --res)
    abort ();

  atomic_fetch_sub (&v, 1);
  if (v != --res)
    abort ();

  atomic_fetch_sub_explicit (&v, 1, memory_order_release);
  if (v != --res)
    abort ();

  atomic_fetch_sub (&v, count + 1);
  if (v != --res)
    abort ();

  atomic_fetch_sub_explicit (&v, count + 1, memory_order_seq_cst);
  if (v != --res)
    abort ();
}

void
test_and ()
{
  v = init;

  atomic_fetch_and (&v, 0);
  if (v != 0)
    abort ();

  v = init;
  atomic_fetch_and_explicit (&v, init, memory_order_consume);
  if (v != init)
    abort ();

  atomic_fetch_and (&v, 0);
  if (v != 0)
    abort ();

  v = ~v;
  atomic_fetch_and_explicit (&v, init, memory_order_release);
  if (v != init)
    abort ();

  atomic_fetch_and (&v, 0);
  if (v != 0)
    abort ();

  v = ~v;
  atomic_fetch_and_explicit (&v, 0, memory_order_seq_cst);
  if (v != 0)
    abort ();
}

void
test_xor ()
{
  v = init;
  count = 0;

  atomic_fetch_xor (&v, count);
  if (v != init)
    abort ();

  atomic_fetch_xor_explicit (&v, ~count, memory_order_consume);
  if (v != 0)
    abort ();

  atomic_fetch_xor (&v, 0);
  if (v != 0)
    abort ();

  atomic_fetch_xor_explicit (&v, ~count, memory_order_release);
  if (v != init)
    abort ();

  atomic_fetch_xor_explicit (&v, 0, memory_order_acq_rel);
  if (v != init)
    abort ();

  atomic_fetch_xor (&v, ~count);
  if (v != 0)
    abort ();
}

void
test_or ()
{
  v = 0;
  count = 19342813113834066795298816wb;

  atomic_fetch_or (&v, count);
  if (v != 19342813113834066795298816wb)
    abort ();

  count *= 2;
  atomic_fetch_or_explicit (&v, count, memory_order_consume);
  if (v != 58028439341502200385896448wb)
    abort ();

  count *= 2;
  atomic_fetch_or (&v, 77371252455336267181195264wb);
  if (v != 135399691796838467567091712wb)
    abort ();

  count *= 2;
  atomic_fetch_or_explicit (&v, 154742504910672534362390528wb,
			    memory_order_release);
  if (v != 290142196707511001929482240wb)
    abort ();

  count *= 2;
  atomic_fetch_or (&v, count);
  if (v != 599627206528856070654263296wb)
    abort ();

  count *= 2;
  atomic_fetch_or_explicit (&v, count, memory_order_seq_cst);
  if (v != 1218597226171546208103825408wb)
    abort ();
}

void
test_exchange (void)
{
  atomic_store (&v, 15794812138349191682564933935017390008wb);
  if (atomic_exchange (&v, -2166613183393424891717146518563613668wb)
      != 15794812138349191682564933935017390008wb
      || atomic_load (&v) != -2166613183393424891717146518563613668wb)
    abort ();
  if (atomic_exchange_explicit (&v, 61251098386268815852902382804483910638wb,
			        memory_order_relaxed)
      != -2166613183393424891717146518563613668wb
      || (atomic_load_explicit (&v, memory_order_acquire)
	  != 61251098386268815852902382804483910638wb))
    abort ();

  count = -2166613183393424891717146518563613668wb;
  if (atomic_compare_exchange_strong (&v, &count,
				      -36677332297536901313774263310237646448wb))
    abort ();
  if (count != 61251098386268815852902382804483910638wb
      || atomic_load (&v) != 61251098386268815852902382804483910638wb)
    abort ();
  if (!atomic_compare_exchange_strong (&v, &count,
				       -36677332297536901313774263310237646448wb))
    abort ();
  if (count != 61251098386268815852902382804483910638wb
      || atomic_load (&v) != -36677332297536901313774263310237646448wb)
    abort ();

  count = -2166613183393424891717146518563613668wb;
  if (atomic_compare_exchange_strong_explicit (&v, &count,
					       73949932022761409003352953944661689416wb,
					       memory_order_seq_cst,
					       memory_order_relaxed))
    abort ();
  if (count != -36677332297536901313774263310237646448wb
      || atomic_load (&v) != -36677332297536901313774263310237646448wb)
    abort ();
  if (!atomic_compare_exchange_strong_explicit (&v, &count,
						73949932022761409003352953944661689416wb,
						memory_order_seq_cst,
						memory_order_seq_cst))
    abort ();
  if (count != -36677332297536901313774263310237646448wb
      || atomic_load (&v) != 73949932022761409003352953944661689416wb)
    abort ();

  count = atomic_load (&v);
  do
    res = count + -82256758205518164043596305502815392646wb;
  while (!atomic_compare_exchange_weak (&v, &count, res));
  if (atomic_load (&v) != -8306826182756755040243351558153703230wb)
    abort ();

  count = atomic_load_explicit (&v, memory_order_acquire);
  do
    res = count + 48855144829609538366772317026461909818wb;
  while (!atomic_compare_exchange_weak_explicit (&v, &count, res,
						 memory_order_relaxed,
						 memory_order_relaxed));
  if (atomic_load (&v) != 40548318646852783326528965468308206588wb)
    abort ();
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 127
  test_fetch_add ();
  test_fetch_sub ();
  test_fetch_and ();
  test_fetch_xor ();
  test_fetch_or ();
  test_add ();
  test_sub ();
  test_and ();
  test_xor ();
  test_or ();
  test_exchange ();
#endif
  return 0;
}
