// PR c++/19739

void Dummy() __attribute__(( , ));
void Dummy() {}

int main (int argc, char **argv)
{
    Dummy();
    return 0;
}
