/* PR tree-optimization/106126 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

char *var_1;
void pool_conda_matchspec() {
  for (; var_1 && *var_1 &&
         *var_1 != '<' && *var_1 != '>' &&
         *var_1 != '!' && *var_1 != '~';)
    if (*var_1 >= 'A' && *var_1 <= 'Z')
      *var_1 += 'A';
}
