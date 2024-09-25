// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }
// Gets ICE 40.

#include <vector>
#include<string>
#include <iostream>

using namespace std;

class ODEsolver
{
    private:
        void eulerODE(vector<double>& y, double& t, double& dt);
        void midpointODE(vector<double>& y, double& t, double& dt);

    protected:
        void (ODEsolver::*useMethod)(vector<double>&, double&, double&);
        void init();

    public:
        ODEsolver();
        void timeloop(vector<double>& y, double ts, double te, double dt);
};


ODEsolver::ODEsolver()
{
  init();
}


void ODEsolver::eulerODE(vector<double>& y, double& t, double& dt)
{
  y[0] = dt * 2.;
}

void ODEsolver::midpointODE(vector<double>& y, double& t, double& dt)
{
  y[0] = dt * 3.;
}



void ODEsolver::init()
{
  ODEsolver::useMethod = &ODEsolver::midpointODE;
}

void ODEsolver::timeloop(vector<double>& y, double ts, double te, double dt)
{
  (ODEsolver::useMethod)(y,ts,dt); // { dg-error "" } should use this->*
}

int main (int nargs, char** args)
{
  ODEsolver solver;
  vector<double> y(2);  double t_start=5.;  double t_end=7.;  double dt=2.;
  solver.timeloop(y,t_start,t_end,dt);
  cout << y[0] << endl;
  return(0);
}
