// PR c++/65880

class Test
{
  public:
    Test();
    ~Test();

    bool barl(void);

  private:
    bool fool(bool (Test::* const *fms)(void));
    bool foo(void);
    bool bar(void);
};

Test::Test()
{
}

Test::~Test()
{
}

bool Test::fool(bool (Test::* const *fms)(void))
{
    bool retval = false;

    int i = 0;
    bool (Test::*f)(void) = fms[i++];

    while (f) {
        retval = (this->*f)();
        if (retval) break;
        f = fms[i++];
    }

    return retval;
}


bool Test::barl(void)
{
    static bool (Test::* const fms[])(void) = {
        &Test::foo,
        &Test::bar,
        0
    };



    return fool(fms);
}


bool Test::foo(void)
{
    return false;
}

bool Test::bar(void)
{
    return true;
}

int main(int argc, const char *argv[])
{
    Test t;
    return t.barl();
}
