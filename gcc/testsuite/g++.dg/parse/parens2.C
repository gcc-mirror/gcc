/* PR c++/8842. */
/* { dg-do compile } */
int main( int argc, char* argv )
{
    int i = 5;
    // This always worked:
    // double l1 =  double(int(i)) / double(int(i));
    // But this used to give a parse error before the `/' token:
    double l2 = (double(int(i)) / double(int(i))); 
}

