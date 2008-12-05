/* { dg-do compile } */
/* { dg-options "-O2 -std=gnu99 -pedantic-errors -mea-to-generic-conversion" } */

/* This is the same code as errors2.c but it should compile cleanly.  If you
   modify this file, you should modify errors2.c as well.  */
#ifndef TYPE
#define TYPE void
#endif

typedef __ea TYPE *ea_ptr_t;
typedef      TYPE *lm_ptr_t;

extern ea_ptr_t ea_ptr;
extern lm_ptr_t lm_ptr;

extern void arg_ea (ea_ptr_t);
extern void arg_lm (lm_ptr_t);

#ifdef NO_CAST
#define EA_CAST(ARG) (ARG)
#define LM_CAST(ARG) (ARG)

#else
#define EA_CAST(ARG) ((ea_ptr_t)(ARG))
#define LM_CAST(ARG) ((lm_ptr_t)(ARG))
#endif

void ea_to_ea_arg (void)
{
  arg_ea (ea_ptr);
}

void ea_to_lm_arg (void)
{
  arg_lm (LM_CAST (ea_ptr));
}

void lm_to_ea_arg (void)
{
  arg_ea (EA_CAST (lm_ptr));
}

void lm_to_lm_arg (void)
{
  arg_lm (lm_ptr);
}

ea_ptr_t ea_to_ea_ret (void)
{
  return ea_ptr;
}

lm_ptr_t ea_to_lm_ret (void)
{
  return LM_CAST (ea_ptr);
}

ea_ptr_t lm_to_ea_ret (void)
{
  return EA_CAST (lm_ptr);
}

lm_ptr_t lm_to_lm_ret (void)
{
  return lm_ptr;
}

void ea_to_ea_store (ea_ptr_t ptr)
{
  ea_ptr = ptr;
}

void ea_to_lm_store (ea_ptr_t ptr)
{
  lm_ptr = LM_CAST (ptr);
}

void lm_to_ea_store (lm_ptr_t ptr)
{
  ea_ptr = EA_CAST (ptr);
}

void lm_to_lm_store (lm_ptr_t ptr)
{
  lm_ptr = ptr;
}
