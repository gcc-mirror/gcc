/* PR middle-end/37669 */
/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */

#define FMT10 "%d%d%d%d%d%d%d%d%d%d"
#define FMT100 FMT10 FMT10 FMT10 FMT10 FMT10 FMT10 FMT10 FMT10 FMT10 FMT10
#define FMT1000 FMT100 FMT100 FMT100 FMT100 FMT100 \
		FMT100 FMT100 FMT100 FMT100 FMT100
#define ARG10 , i, i, i, i, i, i, i, i, i, i
#define ARG100 ARG10 ARG10 ARG10 ARG10 ARG10 ARG10 ARG10 ARG10 ARG10 ARG10
#define ARG1000 ARG100 ARG100 ARG100 ARG100 ARG100 \
		ARG100 ARG100 ARG100 ARG100 ARG100
void foo (char *s, int i, int j)
{
  __builtin___snprintf_chk (s, i, 1, j, FMT1000 ARG1000);
}
