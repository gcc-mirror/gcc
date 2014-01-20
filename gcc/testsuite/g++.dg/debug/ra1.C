/* { dg-options "-fcompare-debug" } */

enum signop { SIGNED, UNSIGNED };
enum tree_code { FOO, BAR };
enum tree_code_class { tcc_type, tcc_other };
extern enum tree_code_class tree_code_type[];

struct tree_base {
  enum tree_code code : 8;
  unsigned unsigned_flag : 1;
};

struct tree_def {
  tree_base base;
  struct {
    int precision;
  } type_common;
};

typedef tree_def *tree;

struct storage_ref
{
  storage_ref (const long *, unsigned int, unsigned int);

  const long *val;
  unsigned int len;
  unsigned int precision;
};

inline storage_ref::storage_ref (const long *val_in,
				 unsigned int len_in,
				 unsigned int precision_in)
  : val (val_in), len (len_in), precision (precision_in)
{
}

struct hwi_with_prec
{
  long val;
  unsigned int precision;
  signop sgn;
};

inline storage_ref
decompose (long *scratch, unsigned int precision,
	   const hwi_with_prec &x)
{
  scratch[0] = x.val;
  if (x.sgn == SIGNED || x.val >= 0 || precision <= sizeof (long) * 8)
    return storage_ref (scratch, 1, precision);
  scratch[1] = 0;
  return storage_ref (scratch, 2, precision);
}

extern void tree_class_check_failed (int) __attribute__ ((__noreturn__));

inline tree
tree_class_check (tree t, const enum tree_code_class cls, int x)
{
  if (tree_code_type[t->base.code] != cls)
    tree_class_check_failed (x);
  return t;
}

tree wide_int_to_tree (tree, const storage_ref &);

tree
build_int_cstu (tree type, unsigned long val)
{
  hwi_with_prec x;
  x.val = val;
  x.precision = tree_class_check (type, tcc_type, 1)->type_common.precision;
  x.sgn = (signop) tree_class_check (type, tcc_type, 2)->base.unsigned_flag;
  long scratch[2];
  return wide_int_to_tree (type, decompose (scratch, x.precision, x));
}
