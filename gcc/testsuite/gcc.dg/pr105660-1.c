/* PR105660
 * { dg-do compile }
 * { dg-options "-std=c17" }
 */

void gatherConservativeVars(int, int, int, int, int, int, int Hnvar, int,
                            int Hnyt, int Hnxyt, int, int Hstep, double[Hnyt],
                            double[Hnvar][Hstep][Hnxyt]);
void gatherConservativeVars(int, int, int, int, int, int, int Hnvar, int, int Hnyt,
                            int Hnxyt, int, int Hstep, double[Hnyt],
                            double[Hnvar][Hstep][Hnxyt]);


