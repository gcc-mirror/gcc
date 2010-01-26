/* { dg-do compile } */
/* { dg-options "-O2 -fipa-type-escape" } */

extern double log10 (double __x);
extern double ceil (double __x);
extern double floor (double __x);
extern void free (void *__ptr);
extern void *my_malloc (unsigned int);
extern int num_rr_nodes;
static float get_cblock_trans (int *num_inputs_to_cblock,
			       int max_inputs_to_cblock,
			       float trans_cblock_to_lblock_buf,
			       float trans_sram_bit);
static float trans_per_mux (int num_inputs, float trans_sram_bit);
void
count_routing_transistors (int num_switch, float R_minW_nmos,
			   float R_minW_pmos)
{
  int *num_inputs_to_cblock;
  int iswitch, i, j, iseg, max_inputs_to_cblock;
  float input_cblock_trans;
  const float trans_sram_bit = 6.;
  float trans_cblock_to_lblock_buf;
  input_cblock_trans =
    get_cblock_trans (num_inputs_to_cblock, max_inputs_to_cblock,
		      trans_cblock_to_lblock_buf, trans_sram_bit);
}

static float
get_cblock_trans (int *num_inputs_to_cblock, int max_inputs_to_cblock,
		  float trans_cblock_to_lblock_buf, float trans_sram_bit)
{
  float *trans_per_cblock;
  float trans_count;
  int i, num_inputs;

  trans_per_cblock =
    (float *) my_malloc ((max_inputs_to_cblock + 1) * sizeof (float));
  for (i = 1; i <= max_inputs_to_cblock; i++)
    trans_per_cblock[i] =
      trans_per_mux (i, trans_sram_bit) + trans_cblock_to_lblock_buf;
  for (i = 0; i < num_rr_nodes; i++)
    {
      num_inputs = num_inputs_to_cblock[i];
      trans_count += trans_per_cblock[num_inputs];
    }
  free (trans_per_cblock);
  return (trans_count);
}

static float
trans_per_mux (int num_inputs, float trans_sram_bit)
{
  int nlevels, ilevel, current_inps;
  float ntrans = 0;

  if (num_inputs <= 1)
    return (0);
  nlevels = ceil (log10 (num_inputs) / log10 (2.) - 0.00001);
  current_inps = num_inputs;
  for (ilevel = 1; ilevel <= nlevels; ilevel++)
    {
      ntrans += 2 * floor (current_inps / 2.);
      current_inps = ceil (current_inps / 2.);
    }
  ntrans += trans_sram_bit * nlevels;
  return (ntrans);
}
