// Bug: g++ parses the declaration of i as a functional cast.
// Build don't link:

void take_int (int arg) { }
 
void
test ()
{
    int (i);

    i = 0;
    take_int (i);
}
