extern void abort (void);

typedef struct {
  long r[(19 + sizeof (long))/(sizeof (long))];
} realvaluetype;

typedef void *tree;

static realvaluetype
real_value_from_int_cst (tree x, tree y)
{
  realvaluetype r;
  int i;
  for (i = 0; i < sizeof(r.r)/sizeof(long); ++i)
    r.r[i] = -1;
  return r;
}

struct brfic_args
{
  tree type;
  tree i;
  realvaluetype d;
};

static void
build_real_from_int_cst_1 (data)
     void * data;
{
  struct brfic_args *args = (struct brfic_args *) data;
  args->d = real_value_from_int_cst (args->type, args->i);
}

int main()
{
  struct brfic_args args;

  __builtin_memset (&args, 0, sizeof(args));
  build_real_from_int_cst_1 (&args);

  if (args.d.r[0] == 0)
    abort ();
  return 0;
}
