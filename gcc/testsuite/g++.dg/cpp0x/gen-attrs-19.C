// PR c++/19739
// { dg-do compile { target c++11 } }

void Dummy() [[ , ]];
void Dummy() {}

int main (int argc, char **argv)
{
    Dummy();
    return 0;
}
