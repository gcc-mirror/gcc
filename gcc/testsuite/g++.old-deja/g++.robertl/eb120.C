// { dg-do run  }
template<double functionToIntegrate(double)>
double integrate(double a, double b, int numSamplePoints)
{
  //    PRECONDITION(numSamplePoints > 1);
    double delta = (b-a) / (numSamplePoints-1);
    double sum = 0.;
    for (int i=0; i < numSamplePoints; ++i)
        sum += functionToIntegrate(a + i*delta);
    return sum * (b-a) / numSamplePoints;
}

inline double myFunction(double x)
{
    return 1 / (1 + x);
}

// Example use
int main() {
double z = integrate<myFunction>(0.0, 1.0, 50);
	return 0 ;
}

