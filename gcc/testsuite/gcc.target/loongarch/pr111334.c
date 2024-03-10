/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned
util_next_power_of_two (unsigned x)
{
  return (1 << __builtin_clz (x - 1));
}

extern int create_vec_from_array (void);

struct ac_shader_args {
    struct {
	unsigned char offset;
	unsigned char size;
    } args[384];
};

struct isel_context {
    const struct ac_shader_args* args;
    int arg_temps[384];
};


void
add_startpgm (struct isel_context* ctx, unsigned short arg_count)
{

  for (unsigned i = 0, arg = 0; i < arg_count; i++)
    {
      unsigned size = ctx->args->args[i].size;
      unsigned reg = ctx->args->args[i].offset;

      if (reg % ( 4 < util_next_power_of_two (size)
		 ? 4 : util_next_power_of_two (size)))
	  ctx->arg_temps[i] = create_vec_from_array ();
    }
}

