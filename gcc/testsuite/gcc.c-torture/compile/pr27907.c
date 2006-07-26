typedef double fann_type;
typedef struct { } _G_fpos64_t;
struct fann_neuron
{
    fann_type value;
}
__attribute__ ((packed));
struct fann_layer
{
    struct fann_neuron *last_neuron;
};
struct fann
{
    struct fann_layer *first_layer;
};
fann_run (struct fann *ann, fann_type * input)
{
  struct fann_layer *layer_it, *layer_it2, *last_layer;
  for (layer_it = ann->first_layer + 1; layer_it != last_layer; layer_it++)
    {
      ((layer_it - 1)->last_neuron - 1)->value = 1;
    }
}
