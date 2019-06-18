
int test6716(int magic);

extern "C" int rt_init();
extern "C" int rt_term();

int main(int argc, char*argv[])
{
    rt_init();
    int rc = test6716(12345);
    rt_term();
    return rc;
}
