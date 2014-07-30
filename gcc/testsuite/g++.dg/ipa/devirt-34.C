/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-devirt"  } */
struct A {virtual int t(){return 42;}};
struct B:A {virtual int t(){return 1;}};
int
t(struct B *b)
{
  struct A *a=b;
  a->t();
}

/* We should guess that the pointer of type B probably points to an instance
   of B or its derivates and exclude A::t from list of likely targets.  */

/* { dg-final { scan-ipa-dump "Targets that are not likely"  "devirt"  } } */
/* { dg-final { scan-ipa-dump "1 speculatively devirtualized"  "devirt"  } } */
/* { dg-final { cleanup-ipa-dump "devirt" } } */
