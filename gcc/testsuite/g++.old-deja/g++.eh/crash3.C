// { dg-do assemble  }
// Origin: Marc Espie <Marc.Espie@liafa.jussieu.fr>
// Used to use -fsjlj-exceptions, but that isn't an option anymore.

extern double f(double a); 

void 
a()
{
        double e;
        double value;

        if (e == 0)
            throw 1;
        value = f(e);
}
