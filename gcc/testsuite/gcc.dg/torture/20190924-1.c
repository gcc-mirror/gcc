/* { dg-do compile } */

struct acct_gather_energy {
   int base_consumed_energy;
   int consumed_energy;
   int previous_consumed_energy;
};
static struct acct_gather_energy xcc_energy;
struct acct_gather_energy *new;
int _get_joules_task(int first)
{
  if (!first && new->previous_consumed_energy)
    first = 1;
  new->base_consumed_energy = new->consumed_energy;
  __builtin_memcpy(&xcc_energy, new, sizeof(struct acct_gather_energy));
  return xcc_energy.base_consumed_energy;
}
