//  PR tree-opt/19768
// tree DSE was removing one store to LL.currentLevel
//  but forgot that since the vop was in an abnormal PHI
//  that we have to update the SSA_NAME which we propagate
//  into the abnormal PHI

// { dg-do compile }
// { dg-options "-O" }

struct LeveLogger
{
  int currentLevel;
};
extern LeveLogger LL;
struct gg
{
  ~gg ( void )
    { LL.currentLevel = 1; }
};
void f(void);
void g ( void )
{
  gg sll;
  {
    gg sll;
    f();
  }
  f();
}
