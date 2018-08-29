/* { dg-do compile } */
/* { dg-options "-O -fno-tree-sra -fdump-tree-fre1" } */

int f();

template<typename predicate>
class noop_t {
    const predicate &pred;
public:
    explicit noop_t(const predicate &p) : pred(p) {}

    int operator()() const { return pred(); }
};

template<typename predicate>
inline noop_t<predicate> noop(const predicate &pred) {
    return noop_t<predicate>(pred);
}

int x()
{
  return (noop(noop(noop(noop(noop(noop(noop(noop(noop(f)))))))))());
}

/* We should optimize this to a direct call.  */

/* { dg-final { scan-tree-dump-times "= f \\(\\);" 1 "fre1" } } */
