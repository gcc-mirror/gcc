/* PR target/101132
   This was triggering an ICE in do_store_flag when compiled with -mmsa -O3. */

/* { dg-do compile } */
/* { dg-options "-mmsa" } */

int r_0, q_0;
void bar() {
  int i;
  for (i = 0; i < 96; i++) {
    r_0 = i << i ? 2 + i : -i;
    q_0 = r_0 > 2 ?: i;
  }
}
