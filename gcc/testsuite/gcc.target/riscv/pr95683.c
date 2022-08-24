/* PR target/95683 */
/* { dg-options "" } */
/* { dg-do compile } */
void a() {
  asm(""
      :
      :
      : "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "t0", "t1", "t2", "t3",
        "t4", "t5", "t6", "ra");
}
