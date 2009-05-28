/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution" } */
#define NULL ((void *)0)

__extension__ typedef __SIZE_TYPE__ size_t;
extern void *foo(size_t nelem, size_t elsize);
extern void bar (char*, ...);

typedef struct alt_state *alt_state_t;
typedef struct state *state_t;

struct alt_state
{
  alt_state_t next_alt_state;
};

static alt_state_t first_free_alt_state = NULL;

static void
free_alt_state (alt_state_t alt_state)
{
  if (alt_state == NULL)
    return;
  alt_state->next_alt_state = first_free_alt_state;
  first_free_alt_state = alt_state;
}

/* The function frees list started with node ALT_STATE_LIST.  */
static void
free_alt_states (alt_state_t alt_states_list)
{
  alt_state_t curr_alt_state;
  alt_state_t next_alt_state;

  for (curr_alt_state = alt_states_list;
       curr_alt_state != NULL;
       curr_alt_state = next_alt_state)
    {
      next_alt_state = curr_alt_state->next_alt_state;
      free_alt_state (curr_alt_state);
    }
}

int 
main (void)
{
  int i;
  alt_state_t state, act_state;

  act_state = state = foo (1, sizeof (struct alt_state));
  for (i = 0; i < 2; i ++)
  {
    act_state->next_alt_state = foo (1, sizeof (struct alt_state));
    act_state = act_state->next_alt_state;
  }

  free_alt_states (state);

  for (act_state = first_free_alt_state;
       act_state != NULL;
       act_state = act_state->next_alt_state)
       bar ("going from %p to %p\n", act_state, act_state->next_alt_state);

  return 0;
}
