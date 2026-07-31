// { dg-options "-fno-lto" }

extern "C" int run_repro(int argc);

int main(int argc, char**)
{
    return run_repro(argc >= 0);
}
