/* PR c/10320
   The function temp was not being emitted in a prerelease of 3.4 20030406. 
   Contributed by pinskia@physics.uc.edu */

static inline void temp();
int main()
{
        temp();
        return 0;
}
static void temp(){}


