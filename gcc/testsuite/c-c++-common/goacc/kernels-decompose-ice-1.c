/* Test OpenACC 'kernels' construct decomposition.  */

/* { dg-additional-options "-fopt-info-omp-all" } */
/* { dg-additional-options "-fchecking --param=openacc-kernels=decompose" } */
/* { dg-ice "TODO" }
   { dg-prune-output "during GIMPLE pass: omplower" } */

/* Reduced from 'kernels-decompose-2.c'.
   (Hopefully) similar instances:
     - 'kernels-decompose-ice-2.c'
     - 'libgomp.oacc-c-c++-common/declare-vla-kernels-decompose-ice-1.c'
     - 'libgomp.oacc-c-c++-common/kernels-decompose-1.c'
*/

int
main ()
{
#define N 10

#pragma acc kernels
  for (int i = 0; i < N; i++) /* { dg-message "note: beginning 'parloops' part in OpenACC 'kernels' region" } */
    ;

  return 0;
}

/*
  In 'gimple' we've got:

      main ()
      {
        int D.2087;
      
        {
          int a[10];
      
          try
            {
              #pragma omp target oacc_kernels map(tofrom:a [len: 40])
                {
                  {
                    int i;
      
                    i = 0;
                    goto <D.2085>;
      [...]

  ..., which in 'omp_oacc_kernels_decompose' we turn into:

      main ()
      {
        int D.2087;
      
        {
          int a[10];
      
          try
            {
              #pragma omp target oacc_data_kernels map(tofrom:a [len: 40])
                {
                  try
                    {
                      {
                        int i;
      
                        #pragma omp target oacc_data_kernels map(alloc:i [len: 4])
                          {
                            try
                              {
                                {
                                  #pragma omp target oacc_kernels async(-1) map(force_present:i [len: 4]) map(force_present:a [len: 40])
                                    {
                                      i = 0;
                                      goto <D.2085>;
      [...]

  ..., which results in ICE in:

    #1  0x0000000000d2247b in lower_omp_target (gsi_p=gsi_p@entry=0x7fffffffbc90, ctx=ctx@entry=0x2c994c0) at [...]/gcc/omp-low.c:11981
    11981                       gcc_assert (offloaded);
    (gdb) list
    11976                         talign = TYPE_ALIGN_UNIT (TREE_TYPE (TREE_TYPE (ovar)));
    11977                       gimplify_assign (x, var, &ilist);
    11978                     }
    11979                   else if (is_gimple_reg (var))
    11980                     {
    11981                       gcc_assert (offloaded);
    11982                       tree avar = create_tmp_var (TREE_TYPE (var));
    11983                       mark_addressable (avar);
    11984                       enum gomp_map_kind map_kind = OMP_CLAUSE_MAP_KIND (c);
    11985                       if (GOMP_MAP_COPY_TO_P (map_kind)
    (gdb) call debug_tree(var)
     <var_decl 0x7ffff7feebd0 i
        type <integer_type 0x7ffff67be5e8 int sizes-gimplified public SI
            size <integer_cst 0x7ffff67a5f18 constant 32>
            unit-size <integer_cst 0x7ffff67a5f30 constant 4>
            align:32 warn_if_not_align:0 symtab:0 alias-set -1 canonical-type 0x7ffff67be5e8 precision:32 min <integer_cst 0x7ffff67a5ed0 -2147483648> max <integer_cst 0x7ffff67a5ee8 2147483647>
            pointer_to_this <pointer_type 0x7ffff67c69d8>>
        used read SI [...]:15:12 size <integer_cst 0x7ffff67a5f18 32> unit-size <integer_cst 0x7ffff67a5f30 4>
        align:32 warn_if_not_align:0 context <function_decl 0x7ffff68eea00 main>>

  Just defusing the 'assert' is not sufficient:

      libgomp: present clause: !acc_is_present (0x7ffe29cba3ec, 4 (0x4))

  TODO Can't the 'omp_oacc_kernels_decompose' transformation be much simpler, such that we avoid the intermediate 'data' if we've got just one compute construct inside it?
  TODO But it's not clear if that'd just resolve one simple instance of the general problem?

*/
