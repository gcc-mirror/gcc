// C++ PR/10476
// Origin: larsbj@gullik.net and bangerth@dealii.org


struct X {
  X();
  X(const X& __str);
};
X const bar();
void foo()
{
  X y;
  (true ? y : bar());
}

