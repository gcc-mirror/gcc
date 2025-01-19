/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O1" "-O2" "-O3" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv64gc_zbb_zbs -mabi=lp64d -O0" } */

struct a {
  unsigned : 29;
  signed : 6;
  signed b : 25;
};

void c() {
  struct a d = {808};
}

/* { dg-final { scan-assembler-not "bseti.*31" } }*/
