// Build don't link:
// Origin: Marc Espie <Marc.Espie@liafa.jussieu.fr>
// Special g++ Options: -fsjlj-exceptions

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
