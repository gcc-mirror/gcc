 /* { dg-do run } */
 /* { dg-options "-O2 -fmodulo-sched -fmodulo-sched-allow-regmoves -fdump-rtl-sms" } */


typedef __SIZE_TYPE__ size_t;
extern void *malloc (size_t);
extern void free (void *);
extern void abort (void);

struct regstat_n_sets_and_refs_t
{
  int sets;
  int refs;
};

struct regstat_n_sets_and_refs_t *regstat_n_sets_and_refs;

struct df_reg_info
{
  unsigned int n_refs;
};

struct df_d
{
  struct df_reg_info **def_regs;
  struct df_reg_info **use_regs;
};
struct df_d *df;

static inline int
REG_N_SETS (int regno)
{
  return regstat_n_sets_and_refs[regno].sets;
}

__attribute__ ((noinline))
     int max_reg_num (void)
{
  return 100;
}

__attribute__ ((noinline))
     void regstat_init_n_sets_and_refs (void)
{
  unsigned int i;
  unsigned int max_regno = max_reg_num ();

  for (i = 0; i < max_regno; i++)
    {
      (regstat_n_sets_and_refs[i].sets = (df->def_regs[(i)]->n_refs));
      (regstat_n_sets_and_refs[i].refs =
       (df->use_regs[(i)]->n_refs) + REG_N_SETS (i));
    }
}

int a_sets[100] =
  { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
  21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
  40, 41, 42,
  43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61,
  62, 63, 64,
  65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83,
  84, 85, 86,
  87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99
};

int a_refs[100] =
  { 0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38,
  40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74, 76,
  78, 80, 82,
  84, 86, 88, 90, 92, 94, 96, 98, 100, 102, 104, 106, 108, 110, 112, 114, 116,
  118, 120,
  122, 124, 126, 128, 130, 132, 134, 136, 138, 140, 142, 144, 146, 148, 150,
  152, 154, 156,
  158, 160, 162, 164, 166, 168, 170, 172, 174, 176, 178, 180, 182, 184, 186,
  188, 190, 192,
  194, 196, 198
};

int
main ()
{
  struct df_reg_info *b[100], *c[100];
  struct df_d df1;
  size_t s = sizeof (struct df_reg_info);
  struct regstat_n_sets_and_refs_t a[100];

  df = &df1;
  regstat_n_sets_and_refs = a;
  int i;

  for (i = 0; i < 100; i++)
    {
      b[i] = (struct df_reg_info *) malloc (s);
      b[i]->n_refs = i;
      c[i] = (struct df_reg_info *) malloc (s);
      c[i]->n_refs = i;
    }

  df1.def_regs = b;
  df1.use_regs = c;
  regstat_init_n_sets_and_refs ();

  for (i = 0; i < 100; i++)
    if ((a[i].sets != a_sets[i]) || (a[i].refs != a_refs[i]))
      abort ();

  for (i = 0; i < 100; i++)
    {
      free (b[i]);
      free (c[i]);
    }

  return 0;
}

/* { dg-final { scan-rtl-dump-times "SMS succeeded" 1 "sms" { target powerpc*-*-* } } } */

