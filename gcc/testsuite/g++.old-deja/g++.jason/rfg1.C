// { dg-do assemble  }
// Bug: g++ parses the declaration of i as a functional cast.

void take_int (int arg) { }
 
void
test ()
{
    int (i);

    i = 0;
    take_int (i);
}
