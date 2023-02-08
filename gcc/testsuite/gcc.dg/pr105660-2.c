/* PR105660
 * { dg-do compile }
 * { dg-options "-Wall -std=c17" }
 */


struct bat_gen_conf_s;
void batch_generator_create2(struct bat_gen_conf_s* config, int D, int N, const long bat_dims[D][N], const long tot_dims[D][N], const long tot_strs[D][N], const _Complex float* data[D]);
void batch_generator_create2(struct bat_gen_conf_s* config, int D, int N, const long bat_dims[D][N], const long tot_dims[D][N], const long tot_strs[D][N], const _Complex float* data[D]);



