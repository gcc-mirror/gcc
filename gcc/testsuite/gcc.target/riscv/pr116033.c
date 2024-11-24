/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv64gv_xtheadmemidx" { target { rv64 } } } */
/* { dg-options "-march=rv32gv_xtheadmemidx" { target { rv32 } } } */

char arr_3[20][20];
void init()
{
  for (int i_0 = 0; i_0 < 20; ++i_0)
    for (int i_1 = 0; i_0 < 20; ++i_0)
      for (int i_1 = 0; i_1 < 20; ++i_0)
        for (int i_1 = 0; i_1 < 20; ++i_1)
          arr_3[i_0][i_1] = i_1;
}

/* { dg-final { scan-assembler-not "vse8.v\t\[a-x0-9\]+,\\(\[a-x0-9\]+\\),\[0-9\]+,\[0-9\]+" } } */
